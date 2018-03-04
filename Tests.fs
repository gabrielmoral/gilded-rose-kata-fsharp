module Tests

open Xunit
open Rules

[<Fact>]
let ``Decrease quality and number of days at the end of the day`` () =
    let item = processItem { SellIn = SellIn 1; Quality = Quality 1; Type = None}

    Assert.Equal(SellIn 0, item.SellIn)
    Assert.Equal(Quality 0, item.Quality)

[<Fact>]
let ``Quality decrease twice if date has passed`` () =
    let item = processItem { SellIn = SellIn 0; Quality = Quality 2; Type = None}

    Assert.Equal(Quality 0, item.Quality)

[<Fact>]
let ``Quality is never negative`` () =
    let item = processItem { SellIn = SellIn 0; Quality = Quality 0; Type = None}

    Assert.Equal(Quality 0, item.Quality)

[<Fact>]
let ``"Aged Brie" increases in Quality the older it gets`` () =
    let item = processItem { SellIn = SellIn 0; Quality = Quality 0; Type = Some AgedBrie}

    Assert.Equal(Quality 1, item.Quality)

[<Fact>]
let ``The Quality of an item is never more than 50`` () =
    let item = processItem { SellIn = SellIn 0; Quality = Quality 50; Type = Some AgedBrie}

    Assert.Equal(Quality 50, item.Quality)

[<Fact>]
let ``"Sulfuras", being a legendary item, never has to be sold or decreases in Quality`` () =
    let item = processItem { SellIn = SellIn 1; Quality = Quality 1; Type = Some Sulfuras}

    Assert.Equal(Quality 1, item.Quality)
    Assert.Equal(SellIn 1, item.SellIn)

[<Fact>]
let ``"Backstage passes" increases quality like Aged Brie`` () =
    let qualityIncresed = processItem { SellIn = SellIn 30; Quality = Quality 1; Type = Some BackStagePass}

    Assert.Equal(Quality 2, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" increases quality by 2 when there are 10 days or less`` () =
    let qualityIncresed = processItem { SellIn = SellIn 10; Quality = Quality 1; Type = Some BackStagePass}

    Assert.Equal(Quality 3, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" increases quality by 3 when there are 5 days or less`` () =
    let qualityIncresed = processItem { SellIn = SellIn 5; Quality = Quality 1; Type = Some BackStagePass}

    Assert.Equal(Quality 4, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" quality drops to 0 after the concert`` () =
    let noQuality = processItem { SellIn = SellIn 0; Quality = Quality 10; Type = Some BackStagePass}

    Assert.Equal(Quality 0, noQuality.Quality)

[<Fact>]
let ``"Conjured" items degrade in Quality twice as fast as normal items`` () =
    let degradedTwice = processItem { SellIn = SellIn 10; Quality = Quality 10; Type = Some Conjured}

    Assert.Equal(Quality 8, degradedTwice.Quality)