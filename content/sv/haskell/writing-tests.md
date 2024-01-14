---
title:    "Haskell: Att skriva tester"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av en utvecklares arbete för att upprätthålla kvaliteten på koden och minska risken för buggar i programmet.

## Så här gör du
Det första steget för att skriva tester är att importera testningsbiblioteket "HUnit" med att skriva ```Haskell import Test.HUnit``` i början av ditt program. Sedan definierar du enskilda enhetstester genom att använda funktionen "TestCase" och kallar sedan på testningarna med hjälp av "test" funktionen. Här är ett exempel på hur en test skulle kunna se ut:

```Haskell
testAddition = TestCase (assertEqual "2 + 2 ska vara 4" 4 (2+2))

testMultiplikation = TestCase (assertEqual "3 * 5 ska vara 15" 15 (3*5))

allaTester = TestList [testAddition, testMultiplikation]

main = runTestTT allaTester
```
Kör detta program och om alla tester passerar, kommer du se utskriften "Cases: 2  Tried: 2  Errors: 0  Failures: 0", vilket visar att dina tester var lyckade.

## Djupdykning
Ett bra tillvägagångssätt för att skriva tester är att följa principen "Arrange, Act, Assert". Det innebär att förbereda testet genom att skapa förutsättningar (Arrange), utföra en handling (Act) och sedan verifiera resultatet (Assert). Ett annat viktigt koncept är "edge cases", det vill säga att testa extremvärden eller ogiltig input för att se hur programmet hanterar dem.

En annan fördel med att skriva tester är att de bidrar till en bättre design av koden. Genom att tänka på testfall under utvecklingsprocessen kan du skapa en mer hållbar och lättunderhållen kod.

## Se även
- [Hängiven¶alla-enhetstest](https://www.haskell.org/tutorial/unit2.html)
- [HUnit-dokumentation](http://hackage.haskell.org/package/HUnit)
- [Test-Driven Development med Haskell](https://www.fpcomplete.com/blog/2016/11/test-driven-development-in-haskell)