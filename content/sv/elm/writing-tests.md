---
title:    "Elm: Skriva tester"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingsprocessen, oavsett vilket programmeringsspråk man använder. Testning hjälper till att säkerställa att vår kod fungerar enligt förväntningarna och minskar risken för buggar i produktion. Följ dessa steg för att lära dig hur man skriver tester i Elm!

## Hur man gör det

För att kunna skriva tester i Elm behöver du först lägga till testning som en dependency i ditt projekt. Detta gör du genom att lägga till "elm-community/elm-test" i din "elm.json" fil. Nu kan du importera "Test" modulen i dina testfiler och använda funktioner som "test" och "expect" för att skapa dina tester.

Nedan finns ett exempel på hur du kan testa en funktion som adderar två tal:

```Elm
import Test exposing (..)

add x y =
    x + y

test "testar adderingsfunktion" <|
    \() ->
        expect (add 2 3) |> toEqual 5
```

Outputen av denna testfil skulle vara "Passed: 1" om funktionen fungerade som förväntat.

## Djupdykning

Elm-test har många funktioner för att underlätta testning. Du kan till exempel använda "describe" för att gruppera dina tester och "fuzz" för att generera slumpmässiga värden för att testa din kod med. Det är också viktigt att testa både positiva och negativa scenarier för att säkerställa att din kod hanterar alla situationer korrekt.

Det är också bra att integrera testning i din Continuous Integration (CI) process för att säkerställa att alla tester körs automatiskt när du pushar till din kodbas. Detta hjälper till att snabbt upptäcka och åtgärda eventuella buggar.

## Se även

- [Elm-test dokumentation](https://package.elm-lang.org/packages/elm-community/elm-test/latest/)
- [Elm-test exempelprojekt](https://github.com/elm-community/elm-test/tree/1.4.3/examples)
- [Elm-test tutorial video](https://www.youtube.com/watch?v=lN7gu5NnTxg)
- [Varför testa koden?](https://medium.com/@pyatyispyatilande/why-test-your-code-e89b50a65b0e) (på ryska)