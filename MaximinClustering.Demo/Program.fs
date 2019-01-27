namespace MaximinClustering.Demo
open System.IO
open MaximinClustering

module Program =
    let getVectorsFromFile inFilepath = 
        use inFile = File.OpenText inFilepath
        [while not inFile.EndOfStream do 
            yield (inFile.ReadLine ()).Split ' ' |> Array.map double]

    [<EntryPoint>]
    let main argv =
        let inFilepath = argv.[0]

        let vectors = getVectorsFromFile inFilepath
        let clusters = Clustering.apply (vectors)
        Plotter.showClusters (clusters, vectors) "Maximin Clustering"

        0