---
title:    "Elm: Radera tecken som matchar ett mönster"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt kan det finnas behov av att ta bort vissa tecken som matchar ett visst mönster. Detta kan vara användbart för att filtrera och rensa data eller för att omformatera text. Genom att lära sig hur man tar bort tecken som matchar ett mönster kan du effektivisera dina programmeringsuppgifter och uppnå mer precision i ditt arbete.

## Hur man gör

Att ta bort tecken som matchar ett mönster kan göras med hjälp av en funktion i Elm som heter `Regex.replace`. Denna funktion tar tre argument: ett reguljärt uttryck, en sträng och en funktion som anger vad som ska ersätta det matchande mönstret. I exemplet nedan kommer vi att ta bort alla siffror från en sträng och ersätta dem med en tom sträng.

```Elm
import Regex

input = "Elm är en fantastiskt språk som är bara 123 ännu bättre"

cleanedInput = Regex.replace (Regex.regex "\\d") input (\_ -> "")

-- Output: "Elm är en fantastiskt språk som är bara ännu bättre"
```

Vi importerar först modulen `Regex` och definierar sedan en sträng `input` med både bokstäver och siffror. Sedan används `Regex.replace` för att ta bort alla siffror från `input` genom att använda ett reguljärt uttryck (`\\d` betyder alla siffror) och ersätta dem med en tom sträng. Detta resulterar i att strängen `cleanedInput` endast innehåller bokstäver.

## Djupdykning

Nu när vi har visat ett enkelt exempel på hur man tar bort tecken som matchar ett mönster, kan vi gå djupare in på hur man skapar reguljära uttryck. Elm stöder PCRE (Perl Compatible Regular Expressions), vilket innebär att du kan använda samma syntax som i många andra programmeringsspråk som stöder reguljära uttryck.

Här är några grundläggande metakaraktärer som kan vara användbara för att skapa reguljära uttryck i Elm:

- `.` matchar vilken karaktär som helst
- `*` matchar noll eller flera gånger
- `+` matchar en eller flera gånger
- `?` matchar noll eller en gång
- `|` används för att separera olika möjliga matchningar
- `()` används för att gruppera mönster och återanvända dem i ersättningsfunktionen

Studiematerial för att lära dig mer om reguljära uttryck i Elm:

- [Elms dokumentation om reguljära uttryck](https://package.elm-lang.org/packages/elm/regex/latest/)
- [RegExr](https://regexr.com/) - ett interaktivt verktyg för att testa och utforska reguljära uttryck
- [RegEx101](https://regex101.com/) - ett annat användbart verktyg för att testa och utforska reguljära uttryck

## Se även

- [Elms reguljära uttryckspaket](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Reguljära uttryck 101: En grundläggande handledning](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions)
- [En komplett guide till reguljära uttryck](https://www.regular-expressions.info/)