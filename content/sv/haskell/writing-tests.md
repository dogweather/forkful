---
title:                "Haskell: Att skriva tester"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Innan vi hoppar in i detaljerna av hur man skriver tester i Haskell, låt oss ta en titt på varför det är viktigt att göra det. Att skriva tester hjälper till att säkerställa att vårt program fungerar korrekt och att eventuella buggar upptäcks tidigt. Det är ett viktigt steg i utvecklingsprocessen som bidrar till att säkra kvaliteten på koden.

## Hur man gör

För att skapa tester i Haskell, behöver vi först importera "Hspec" biblioteket. Därefter kan vi definiera våra tester genom att använda funktionen "describe" och specificera vilken del av koden vi vill testa. Sedan använder vi "it" funktionen för att beskriva vad som förväntas hända i vårt test och skriva vår kod. Låt oss se ett exempel nedan med en funktion som lägger till två tal:

```Haskell
describe "add" $ do
  it "returns the sum of two numbers" $ do
    add 2 3 `shouldBe` 5
```

I denna kodspecifikation testar vi funktionen "add" och förväntar oss att den ska returnera rätt summa för två givna tal. Vi använder "shouldBe" för att jämföra det faktiska resultatet med det förväntade resultatet. Om de inte matchar, kommer vårt test att misslyckas och vi måste fixa vår kod.

## Djupdykning

När vi skriver tester i Haskell, kan vi använda många olika funktioner och metoder för att säkerställa att vår kod fungerar som den ska. Vi kan använda "shouldBe" för att testa likhet, "shouldNotBe" för att testa olikhet, och "shouldSatisfy" för att testa en allmän sats. Vi kan också använda "pending" för att markera tester som ännu inte är implementerade och "after" för att köra kod efter tester.

Det finns också många andra bibliotek som kan hjälpa oss att skriva och köra tester i Haskell, som "QuickCheck" för slumpmässiga testscenarier och "HUnit" för enkel enhetstestning.

## Se också

Här är några användbara resurser för att lära dig mer om tester i Haskell:

- Hspec dokumentation: https://hackage.haskell.org/package/hspec
- "Testing in Haskell" guide: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
- "A tutorial on Hspec" blogginlägg: https://vaibhavsagar.com/blog/2018/06/11/haskell-testing/

Tveka inte att utforska dessa resurser och börja implementera tester i din egen Haskell-kod för att skapa en säkrare och buggfri programvara. Lycka till!