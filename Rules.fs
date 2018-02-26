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
    let exceededMinQuality = item.Quality - quality <= minQuality
    if exceededMinQuality then { item with Quality = minQuality }
    else { item with Quality = item.Quality - quality }

let noQuality item = { item with Quality = 0 }

let (|AfterSellIn|_|) x = if x = 0 then Some AfterSellIn else None
let (|BeforeSellIn|_|) x = if x <= 5 then Some BeforeSellIn else None  
let (|LongBeforeSellIn|_|) x = if x <= 10 then Some LongBeforeSellIn else None

let agedBrieQuality item =
    item |> increaseQualityBy 1

let backStagePassQuality item =
    let calculator = match item.SellIn with
                     | AfterSellIn -> noQuality
                     | BeforeSellIn -> increaseQualityBy 3
                     | LongBeforeSellIn -> increaseQualityBy 2
                     | _ -> increaseQualityBy 1
    item |> calculator

let calculateItemQuality item =
    let calculator = match item.SellIn with
                     | AfterSellIn -> decreaseQualityBy 2
                     | _ -> decreaseQualityBy 1
    item |> calculator

let decreaseDate item = 
    if item.SellIn = 0 then item
    else { item with SellIn = item.SellIn - 1 }

let processItem item =
    let processor = match item.Type with
                    | Some Sulfuras -> id
                    | Some AgedBrie -> decreaseDate >> agedBrieQuality
                    | Some BackStagePass -> decreaseDate >> backStagePassQuality
                    | None -> decreaseDate >> calculateItemQuality
    
    item |> processor