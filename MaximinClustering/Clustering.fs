module MaximinClustering.Clustering
open System
open System.Collections.Generic

let private nthOfList list index = List.item index list

let private distance (a: double []) (b: double []) = 
    let sqr x = pown x 2
    seq {for dimension in 0..(a.Length - 1) -> a.[dimension] - b.[dimension] |> sqr} 
    |> Seq.sum 
    |> sqrt

let private formClusters (vectors: double [] list) (centroids: double [] seq) =
    let newListWithElement element = 
        let list = new List<int> ()
        list.Add element
        list
    
    let clusters = new Dictionary<double [], List<int>> ()
    for index, vector in vectors |> Seq.indexed do
        let correspondingCentroid = centroids |> Seq.minBy (distance vector)
        match clusters.TryGetValue correspondingCentroid with
        | true, clusterVectors -> clusterVectors.Add index
        | _ -> clusters.[correspondingCentroid] <- newListWithElement index
    clusters

let private averageDistanceBetween (centroids: ICollection<double []>) = 
    if centroids.Count > 1 then
        centroids 
        |> Seq.indexed
        |> Seq.collect (fun (index, centroid) -> 
            seq {for anotherCentroid in centroids |> Seq.skip index do 
                    yield distance anotherCentroid centroid})
        |> Seq.average
    else
        double 0

let private getNextCentroid (vectors: double [] list) (clusters: IDictionary<double [], List<int>>) = 
    let maxByDistanceFromCentroid centroid clusterVectors =
        clusterVectors
        |> Seq.map (fun vectorIndex -> (vectorIndex, distance vectors.[vectorIndex] centroid)) 
        |> Seq.maxBy snd

    let maxVectorIndex = clusters 
                        |> Seq.map (fun cluster -> cluster.Value |> maxByDistanceFromCentroid cluster.Key)
                        |> Seq.maxBy snd
    if snd maxVectorIndex > (averageDistanceBetween clusters.Keys / 2.0) then
        fst maxVectorIndex |> nthOfList vectors |> Some
    else
        None

let apply (vectors: double [] list) =
    let rec getClusters currentCentroid (centroids: ICollection<double []>) =
        centroids.Add currentCentroid
        let clusters = formClusters vectors centroids
        match getNextCentroid vectors clusters with
        | Some nextCentroid -> getClusters nextCentroid centroids
        | _ -> clusters

    let random = new Random ()
    let initialCentroid = random.Next (vectors.Length - 1) |> nthOfList vectors
    
    new List<double []> () |> getClusters initialCentroid