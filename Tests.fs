module Tests

open Xunit
open Rules

[<Fact>]
let ``Decrease quality and number of days at the end of the day`` () =
    let item = processItem { SellIn = 1; Quality = 1; Type = None}

    Assert.Equal(0, item.SellIn)
    Assert.Equal(0, item.Quality)

[<Fact>]
let ``Quality decrease twice if date has passed`` () =
    let item = processItem { SellIn = 0; Quality = 2; Type = None}

    Assert.Equal(0, item.Quality)

[<Fact>]
let ``Quality is never negative`` () =
    let item = processItem { SellIn = 0; Quality = 0; Type = None}

    Assert.Equal(0, item.Quality)

[<Fact>]
let ``"Aged Brie" increases in Quality the older it gets`` () =
    let item = processItem { SellIn = 0; Quality = 0; Type = Some AgedBrie}

    Assert.Equal(1, item.Quality)

[<Fact>]
let ``The Quality of an item is never more than 50`` () =
    let item = processItem { SellIn = 0; Quality = 50; Type = Some AgedBrie}

    Assert.Equal(50, item.Quality)

[<Fact>]
let ``"Sulfuras", being a legendary item, never has to be sold or decreases in Quality`` () =
    let item = processItem { SellIn = 1; Quality = 1; Type = Some Sulfuras}

    Assert.Equal(1, item.Quality)
    Assert.Equal(1, item.SellIn)

[<Fact>]
let ``"Backstage passes" increases quality like Aged Brie`` () =
    let qualityIncresed = processItem { SellIn = 30; Quality = 1; Type = Some BackStagePass}

    Assert.Equal(2, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" increases quality by 2 when there are 10 days or less`` () =
    let qualityIncresed = processItem { SellIn = 10; Quality = 1; Type = Some BackStagePass}

    Assert.Equal(3, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" increases quality by 3 when there are 5 days or less`` () =
    let qualityIncresed = processItem { SellIn = 5; Quality = 1; Type = Some BackStagePass}

    Assert.Equal(4, qualityIncresed.Quality)

[<Fact>]
let ``"Backstage passes" quality drops to 0 after the concert`` () =
    let noQuality = processItem { SellIn = 0; Quality = 10; Type = Some BackStagePass}

    Assert.Equal(0, noQuality.Quality)