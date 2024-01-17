---
title:                "Radering av tecken som matchar ett mönster"
html_title:           "Haskell: Radering av tecken som matchar ett mönster"
simple_title:         "Radering av tecken som matchar ett mönster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att ta bort tecken som matchar ett mönster är en vanlig uppgift som programmerare behöver göra för att rensa textsträngar från oönskade tecken. Det kan till exempel handla om att ta bort alla mellanslag eller siffror från en textsträng. Det kan också vara ett sätt att filtrera bort icke-önskvärda tecken för att förbereda data för vidare bearbetning. Oavsett anledning, är att ta bort matchande tecken ett praktiskt verktyg för att hantera textbehandling i Haskell.

## Så här gör du:
Här är två enkla exempel på hur du kan ta bort tecken som matchar ett mönster i Haskell:

```Haskell
-- Ta bort alla mellanslag från en textsträng
deletePattern ' ' "Hej på dig" 

Output: "Hejpådig"

-- Ta bort alla siffror från en textsträng
deletePattern isDigit "1+2=3"

Output: "+="
```

Som du kan se, tar funktionen `deletePattern` emot två argument - det första är det önskade mönstret som du vill ta bort, och det andra är textsträngen som ska rensas. Du kan använda inbyggda funktioner som `isDigit` eller ange ditt eget tecken som argument.

## Djupdykning:
Att ta bort tecken som matchar ett mönster är inte något unikt för Haskell - det finns andra programmeringsspråk som också har liknande funktioner, till exempel Python. En annan vanlig metod för att rensa textsträngar är att använda regex, vilket står för "regular expressions", eller reguljära uttryck på svenska. Regex ger mer avancerade mönstermatchningar och är ett kraftfullt verktyg för textbehandling.

När det kommer till implementationen av att ta bort matchande tecken i Haskell, används vanligtvis en loop eller rekursion för att gå igenom varje tecken i textsträngen och jämföra det med det givna mönstret. Om tecknet matchar, tas det bort från strängen och den rensade strängen returneras som output.

## Se även:
- Officiell Haskell dokumentation om [Text Handling](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.1.0/Data-Text.html)
- En grundläggande guide till [Regular Expressions](https://www.regular-expressions.info/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - En underhållande och lättläst introduktion till Haskell för nybörjare