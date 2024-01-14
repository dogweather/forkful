---
title:    "Elm: Att hitta längden på en sträng"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Hej alla läsare! I denna bloggpost kommer vi att prata om hur man hittar längden på en sträng med hjälp av programmeringsspråket Elm. Att kunna hitta längden på en sträng är en grundläggande funktion som kan vara användbar i många olika sammanhang. Låt oss dyka in i ämnet!

## Varför

Du kanske undrar varför man ens skulle behöva hitta längden på en sträng. Svaret är enkelt - det finns många tillfällen där det kan vara användbart att veta hur många tecken en sträng innehåller. Det kan till exempel vara när man vill kontrollera om en sträng är för lång för att visas på en viss plats, eller när man vill göra en analys av data som är sparade som en sträng. 

## Hur man gör det

För att hitta längden på en sträng i Elm, behöver vi använda funktionen `String.length`. Låt oss titta på ett exempel:

```Elm
mySträng = "Hej alla läsare!"

length = String.length mySträng

element.text "Längden på strängen är " ++ String.fromInt length
```

I det här exemplet skapar vi först en variabel `mySträng` med en sträng som innehåller ett meddelande till våra läsare. Sedan använder vi funktionen `String.length` för att hitta längden på strängen och spara den i variabeln `length`. Sedan skriver vi ut resultatet med `element.text` och `String.fromInt` för att konvertera längden till en sträng. När vi kör programmet kommer vi att se följande resultat:

> Längden på strängen är 17

Som du ser är det enkelt att hitta längden på en sträng med hjälp av Elm!

## Djupdykning

För att förstå hur funktionen `String.length` fungerar bakom kulisserna, kan vi titta på dess kod i Elm-källkoden. Detta är dock en djupdykning som är mer relevant för mer avancerade Elm-programmerare. Det viktiga är att du förstår hur man använder funktionen och dess syfte.

Se även:

- [Elm dokumentation om strängar](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-källkod för funktionen String.length](https://github.com/elm/core/blob/1.0.5/src/String.elm#L102)

Tack för att du läste denna bloggpost om att hitta längden på en sträng i Elm. Jag hoppas att du har lärt dig något nytt och att det kan vara till hjälp i dina framtida Elm-projekt. Ha det bra! 

## Se även

- [Elm dokumentation om strängar](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-källkod för funktionen String.length](https://github.com/elm/core/blob/1.0.5/src/String.elm#L102)