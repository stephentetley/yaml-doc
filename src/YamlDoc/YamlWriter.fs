// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace YamlDoc

// Open explicitly

module YamlWriter = 
    
    open System.IO

    open SLFormat

    open YamlDoc.Yaml


    type YamlWriter<'a> = 
        YamlWriter of (int -> StreamWriter -> 'a * int)

    let inline private apply1 (ma: YamlWriter<'a>) 
                              (state:int)
                              (handle: StreamWriter) : 'a * int = 
        let (YamlWriter f) = ma in f state handle

    let inline mreturn (x:'a) : YamlWriter<'a> = 
        YamlWriter <| fun st _ -> (x, st)


    let inline private bindM (ma: YamlWriter<'a>) 
                        (f :'a -> YamlWriter<'b>) : YamlWriter<'b> =
        YamlWriter <| fun st handle  -> 
            let (x, st1) = apply1 ma st handle 
            apply1 (f x) st1 handle 

    /// Haskell's (>>)
    let inline private combineM (mfirst:YamlWriter<'a>) 
                                (msecond:YamlWriter<'b>) : YamlWriter<'b> = 
        YamlWriter <| fun st handle -> 
            let (_, st1) =  apply1 mfirst st handle
            apply1 msecond st1 handle


    let inline private delayM (fn:unit -> YamlWriter<'a>) : YamlWriter<'a> = 
        bindM (mreturn ()) fn 

    type YamlWriterBuilder() = 
        member self.Return x            = mreturn x
        member self.Bind (p,f)          = bindM p f
        member self.Combine (ma,mb)     = combineM ma mb
        member self.Delay fn            = delayM fn
        member self.ReturnFrom(ma)      = ma


    let (yamlWriter:YamlWriterBuilder) = new YamlWriterBuilder()

    // ****************************************************
    // Run

    let runYamlWriter (outPath:string) (ma:YamlWriter<'a>) : 'a = 
        use sw = new StreamWriter(outPath)
        apply1 ma 0 sw |> fst

    let tellYaml (value:Yaml) : YamlWriter<unit> = 
        YamlWriter <| fun st handle ->
            let text = Pretty.render 100 value
            handle.WriteLine text
            ((), st)


    // ****************************************************
    // Monadic operations


    /// fmap 
    let fmapM (fn:'a -> 'b) (ma:YamlWriter<'a>) : YamlWriter<'b> = 
        YamlWriter <| fun state handle -> 
           let (a, st1) =  apply1 ma state handle in (fn a, st1)
           

    /// Implemented in CPS 
    let mapM (mf: 'a -> YamlWriter<'b>) 
             (source:'a list) : YamlWriter<'b list> = 
        YamlWriter <| fun state handle -> 
            let rec work (st1:int) (xs:'a list) (cont : int -> 'b list -> 'b list * int) = 
                match xs with
                | [] -> cont st1 []
                | y :: ys -> 
                    let (ans1, st2) = apply1 (mf y) st1 handle
                    work st2 ys (fun st3 anslist ->
                    cont st3 (ans1::anslist))
            work state source (fun s ans -> (ans, s))


    /// Implemented in CPS 
    let mapMz (mf: 'a -> YamlWriter<'b>) 
              (source:'a list) : YamlWriter<unit> = 
        YamlWriter <| fun state handle -> 
            let rec work (st1:int) (xs:'a list) (cont : int -> unit * int) = 
                match xs with
                | [] -> cont st1
                | y :: ys -> 
                    let (_, st2) = apply1 (mf y) st1 handle
                    work st2 ys (fun st3 ->
                    cont st3)
            work state source (fun s -> ((), s))

    let replicateM (count:int) (ma:YamlWriter<'a>) : YamlWriter<'a list> = 
        YamlWriter <| fun state handle -> 
            let rec work (st1:int) (i:int) (cont : int -> 'a list -> 'a list * int) = 
                if i <= 0 then 
                    cont st1 []
                else
                    let (ans1, st2) = apply1 ma st1 handle
                    work st2 (i-1) (fun st3 anslist ->
                    cont st3 (ans1::anslist))
            work state count (fun s ans -> (ans, s))


    let replicateMz (count:int) (ma:YamlWriter<'a>) : YamlWriter<unit> = 
        YamlWriter <| fun state handle -> 
            let rec work (st1:int) (i:int) (cont : int -> unit * int) = 
                if i <= 0 then 
                    cont st1
                else
                    let (_, st2) = apply1 ma st1 handle
                    work st2 (i-1) (fun st3 ->
                    cont st3)
            work state count (fun s -> ((), s))

    // liftM (which is fmap)
    let liftM (fn:'a -> 'x) (ma:YamlWriter<'a>) : YamlWriter<'x> = 
        fmapM fn ma

    let liftM2 (fn:'a -> 'b -> 'x) 
               (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) : YamlWriter<'x> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            return (fn a b)
        }

    let liftM3 (fn:'a -> 'b -> 'c -> 'x) 
               (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) : YamlWriter<'x> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            let! c = mc
            return (fn a b c)
        }

    let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) 
               (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) : YamlWriter<'x> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            return (fn a b c d)
        }


    let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x) 
               (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) 
               (me:YamlWriter<'e>) : YamlWriter<'x> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            return (fn a b c d e)
        }

    let liftM6 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) 
               (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) 
               (me:YamlWriter<'e>) 
               (mf:YamlWriter<'f>) : YamlWriter<'x> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            let! f = mf
            return (fn a b c d e f)
        }


    let tupleM2 (ma:YamlWriter<'a>) 
                (mb:YamlWriter<'b>) : YamlWriter<'a * 'b> = 
        liftM2 (fun a b -> (a,b)) ma mb

    let tupleM3 (ma:YamlWriter<'a>) 
                (mb:YamlWriter<'b>) 
                (mc:YamlWriter<'c>) : YamlWriter<'a * 'b * 'c> = 
        liftM3 (fun a b c -> (a,b,c)) ma mb mc

    let tupleM4 (ma:YamlWriter<'a>) 
                (mb:YamlWriter<'b>) 
                (mc:YamlWriter<'c>) 
                (md:YamlWriter<'d>) : YamlWriter<'a * 'b * 'c * 'd> = 
        liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

    let tupleM5 (ma:YamlWriter<'a>) 
                (mb:YamlWriter<'b>) 
                (mc:YamlWriter<'c>) 
                (md:YamlWriter<'d>) 
                (me:YamlWriter<'e>) : YamlWriter<'a * 'b * 'c * 'd * 'e> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

    let tupleM6 (ma:YamlWriter<'a>) 
                (mb:YamlWriter<'b>) 
                (mc:YamlWriter<'c>) 
                (md:YamlWriter<'d>) 
                (me:YamlWriter<'e>) 
                (mf:YamlWriter<'f>) : YamlWriter<'a * 'b * 'c * 'd * 'e * 'f> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) ma mb mc md me mf

    let pipeM2 (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (fn:'a -> 'b -> 'x) : YamlWriter<'x> = 
        liftM2 fn ma mb

    let pipeM3 (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (fn:'a -> 'b -> 'c -> 'x) : YamlWriter<'x> = 
        liftM3 fn ma mb mc

    let pipeM4 (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'x) : YamlWriter<'x> = 
        liftM4 fn ma mb mc md

    let pipeM5 (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) 
               (me:YamlWriter<'e>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e ->'x) : YamlWriter<'x> = 
        liftM5 fn ma mb mc md me

    let pipeM6 (ma:YamlWriter<'a>) 
               (mb:YamlWriter<'b>) 
               (mc:YamlWriter<'c>) 
               (md:YamlWriter<'d>) 
               (me:YamlWriter<'e>) 
               (mf:YamlWriter<'f>) 
               (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) : YamlWriter<'x> = 
        liftM6 fn ma mb mc md me mf


    /// Haskell Applicative's (<*>)
    let apM (mf:YamlWriter<'a ->'b>) (ma:YamlWriter<'a>) : YamlWriter<'b> = 
        yamlWriter { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }


    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (ma:YamlWriter<'a>) (mb:YamlWriter<'b>) : YamlWriter<'a> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            return a
        }

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (ma:YamlWriter<'a>) (mb:YamlWriter<'b>) : YamlWriter<'b> = 
        yamlWriter { 
            let! a = ma
            let! b = mb
            return b
        }


    let kleisliL (mf:'a -> YamlWriter<'b>)
                 (mg:'b -> YamlWriter<'c>)
                 (source:'a) : YamlWriter<'c> = 
        yamlWriter { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf:'b -> YamlWriter<'c>)
                 (mg:'a -> YamlWriter<'b>)
                 (source:'a) : YamlWriter<'c> = 
        yamlWriter { 
            let! b = mg source
            let! c = mf b
            return c
        }

    // ****************************************************
    // Monadic operators

    /// Bind operator
    let ( >>= ) (ma:YamlWriter<'a>) 
                (fn:'a -> YamlWriter<'b>) : YamlWriter<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn:'a -> YamlWriter<'b>) 
                (ma:YamlWriter<'a>) : YamlWriter<'b> = 
        bindM ma fn


    /// Operator for fmap.
    let ( |>> ) (ma:YamlWriter<'a>) (fn:'a -> 'b) : YamlWriter<'b> = 
        fmapM fn ma

    /// Flipped fmap.
    let ( <<| ) (fn:'a -> 'b) (ma:YamlWriter<'a>) : YamlWriter<'b> = 
        fmapM fn ma


    /// Operator for seqL
    let (.>>) (ma:YamlWriter<'a>) 
              (mb:YamlWriter<'b>) : YamlWriter<'a> = 
        seqL ma mb

    /// Operator for seqR
    let (>>.) (ma:YamlWriter<'a>) 
              (mb:YamlWriter<'b>) : YamlWriter<'b> = 
        seqR ma mb



    /// Operator for kleisliL
    let (>=>) (mf : 'a -> YamlWriter<'b>)
              (mg : 'b -> YamlWriter<'c>)
              (source:'a) : YamlWriter<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> YamlWriter<'c>)
              (mg : 'a -> YamlWriter<'b>)
              (source:'a) : YamlWriter<'c> = 
        kleisliR mf mg source