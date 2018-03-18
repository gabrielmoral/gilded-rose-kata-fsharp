module Rules

type Quality = Quality of int with
    static member (+) (Quality q, Quality q') = Quality (q + q')
    static member (-) (Quality q, Quality q') = Quality (q - q')

type SellIn = SellIn of int with
    static member (-) (SellIn s, SellIn s') = SellIn (s - s')
    

type Type = AgedBrie | Sulfuras | BackStagePass | Conjured | Generic

type Item = {
    SellIn : SellIn
    Quality : Quality
    Type : Type
}

let increaseQualityBy quality item = 
    let maxQuality = Quality 50
    let exceededMaxQuality = item.Quality + quality >= maxQuality
    if exceededMaxQuality then { item with Quality = maxQuality }
    else { item with Quality = item.Quality + quality }

let decreaseQualityBy quality item =
    let minQuality = Quality 0
    let exceededMinQuality = item.Quality - quality <= minQuality
    if exceededMinQuality then { item with Quality = minQuality }
    else { item with Quality = item.Quality - quality }

let noQuality item = { item with Quality = Quality 0 }

let (|AfterSellIn|_|) x = if x = SellIn 0 then Some AfterSellIn else None
let (|BeforeSellIn|_|) x = if x <= SellIn 5 then Some BeforeSellIn else None  
let (|LongBeforeSellIn|_|) x = if x <= SellIn 10 then Some LongBeforeSellIn else None

let agedBrieQuality item =
    item |> increaseQualityBy (Quality 1)

let backStagePassQuality item =
    let calculator = match item.SellIn with
                     | AfterSellIn -> noQuality
                     | BeforeSellIn -> increaseQualityBy (Quality 3)
                     | LongBeforeSellIn -> increaseQualityBy (Quality 2)
                     | _ -> increaseQualityBy (Quality 1)
    item |> calculator

let genericQuality item =
    let calculator = match item.SellIn with
                     | AfterSellIn -> decreaseQualityBy (Quality 2)
                     | _ -> decreaseQualityBy (Quality 1)
    item |> calculator

let conjuredQuality item =
    item |> decreaseQualityBy (Quality 2)

let decreaseDate item = 
    if item.SellIn = SellIn 0 then item
    else { item with SellIn = item.SellIn - SellIn 1 }

let processItem item =
    let qualityCalculator = match item.Type with
                            | Sulfuras -> id
                            | AgedBrie -> agedBrieQuality
                            | BackStagePass -> backStagePassQuality
                            | Conjured -> conjuredQuality
                            | Generic -> genericQuality

    let sellInCalculator = match item.Type with
                            | Sulfuras -> id
                            | _ -> decreaseDate
    
    item |> (sellInCalculator >> qualityCalculator)