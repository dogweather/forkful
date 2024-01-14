---
title:                "Elm: Sammanfogning av strängar"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en essentiell färdighet i Elm programmering. Genom att kombinera flera strängar till en enda kan du skapa dynamiskt innehåll, som till exempel meddelanden eller användarprofiler. Det är också ett sätt att spara utrymme och undvika upprepning av kod. 

## Så här gör du

För att sammanslå strängar i Elm, använder vi funktionen `String.concat`. Låt oss ta en titt på ett exempel:

```Elm
resultat = String.concat ["Hej", " ", "världen"]
```

I det här exemplet sätter vi ihop tre strängar, "Hej", ett tomrum och "världen". Resultatet blir en enda sträng, "Hej världen". Låt oss nu se vad som händer när vi sätter ihop flera strängar i en funktion:

```Elm
minFunktion str1 str2 str3 =
  String.concat [str1, str2, str3]
````

När vi anropar funktionen med `minFunktion "Hej" " " "världen"`, får vi samma resultat som i det första exemplet. Men nu kan vi enkelt anropa funktionen med olika kombinationer av strängar och få olika resultat.

## Djupdykning

När det kommer till sammanslagning av strängar i Elm, finns det några saker att hålla i åtanke. För det första, funktionen `String.concat` kan endast ta emot en lista av strängar som argument. Om du vill sätta ihop en annan typ av data, som till exempel en int eller en float, måste du först konvertera den till en sträng med hjälp av funktionen `String.fromInt` eller `String.fromFloat`.

För det andra, var noga med att tänka på ordningen som strängarna läggs ihop. Om du till exempel har en lista av namn och vill skapa ett meddelande som innehåller dessa namn, måste du se till att namnen hamnar i rätt ordning så att meddelandet inte blir bakvänt.

## Se även

- [Elm dokumentation om sammanslagning av strängar](https://package.elm-lang.org/packages/elm/core/latest/String#concat)
- [Elm dokumentation om konvertering av data till strängar](https://package.elm-lang.org/packages/elm/core/latest/String#fromInt)
- [Officiell Elm-hemsida](https://elm-lang.org/)