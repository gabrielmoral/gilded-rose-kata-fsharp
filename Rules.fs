module Rules

type Quality = int
type SellIn = int

type Type = AgedBrie | Sulfuras | BackStagePass | Conjured

type Item = {
    SellIn : SellIn
    Quality : Quality
    Type : Type option
}

let increaseQualityBy quality item = 
    let maxQuality = 50
    let exceededMaxQuality = item.Quality + quality >= maxQuality
    if exceededMaxQuality then { item with Quality = maxQuality }
    else { item with Quality = item.Quality + quality }

let decreaseQualityBy quality item =
    let minQuality = 0
    let exceededMinQuality = item.Quality - minQuality <= minQuality
    if exceededMinQuality then { item with Quality = minQuality }
    else { item with Quality = item.Quality - quality }

let (|After|_|) x = if x = 0 then Some After else None
let (|Before|_|) x = if x <= 5 then Some Before else None  
let (|LongBefore|_|) x = if x <= 10 then Some LongBefore else None  

let calculateQuality item = 
    match item.Type with
    | Some AgedBrie -> increaseQualityBy 1
    | Some BackStagePass -> 
        match item.SellIn with
        | After -> decreaseQualityBy item.Quality
        | Before -> increaseQualityBy 3
        | LongBefore -> increaseQualityBy 2
        | _ -> increaseQualityBy 1
    | None ->
        match item.SellIn with
        | 0 -> decreaseQualityBy 2
        | _ -> decreaseQualityBy 1

let decreaseDate item = { item with SellIn = item.SellIn - 1 }

let processItem item =
    let processor = match item.Type with
                    | Some Sulfuras -> id
                    | _ -> decreaseDate >> calculateQuality item
    
    item |> processor