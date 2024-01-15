---
title:                "Att skriva tester"
html_title:           "Haskell: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Det finns flera anledningar till varför det är viktigt att skriva tester när du programmerar i Haskell. För det första hjälper det till att säkerställa att koden fungerar som den ska och minimerar risken för buggar. Det ger också en bättre förståelse för hur koden fungerar och hur den påverkar andra delar av programmet.

## Hur man gör

För att börja skriva tester i Haskell behöver du ett testbibliotek som heter Hspec. Detta bibliotek gör det möjligt att skapa enhetstester för din kod. 

För att använda Hspec behöver du först importera det i din kod genom att skriva `import Test.Hspec` i början av filen. Sedan kan du använda `describe` för att beskriva en specifik funktion som du vill testa och `it` för att specificera vad du förväntar dig att funktionen ska göra. Här är ett exempel på en test som kontrollerar om funktionen `square` beräknar korrekt:

```Haskell
describe "Square function" $ do
  it "Should return the square of a number" $ do
    square 4 `shouldBe` 16
```

För att köra testerna använder du kommandot `runhaskell <filnamn>`. Om alla tester passerar kommer du att se följande output:

```
Square function
   Should return the square of a number
```

## Djupdykning

När du skriver tester är det viktigt att täcka både positiva och negativa fall. Det betyder att du behöver testa både förväntade och oväntade input för att säkerställa att din kod kan hantera alla möjliga scenarion. Det är också viktigt att skriva testerna så tidigt som möjligt i utvecklingsprocessen för att minska risken för buggar.

En annan viktig del av att skriva tester är att hålla dem uppdaterade när du gör ändringar i koden. Om du lägger till ny funktionalitet eller ändrar befintlig kod behöver du också uppdatera dina tester för att säkerställa att allt fortfarande fungerar som det ska.

## Se även

- [Hspec Hackage](https://hackage.haskell.org/package/hspec)
- [Hspec GitHub](https://github.com/hspec/hspec)
- [Haskell Testing for Beginners](https://medium.com/dev-genius/haskell-testing-for-beginners-e10eabebf2f1)