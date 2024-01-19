---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Visar du hur man extraherar understrängar med Haskell? Understrängar är delar av en sträng, och extrahering av dessa är nyckeln till textmanipulering. Detta behövs framförallt när man skapar sökfunktioner och skript som analyserar textdata.

## Såhär:

Här är några grundläggande sätt att extrahera understrängar i Haskell:

Använda `take`, `drop` och `slice`:

```Haskell
let str = "Hejsan, Världen!"
take 3 str -- "Hej"
drop 7 str -- ", Världen!"
slice 0 10 str -- "Hejsan, Vä"
```

Viktigt att notera är att indexeringen börjar från 0.

## Fördjupning

Innan substrängsextraktion blev standard i Haskell, var vanliga sätt att lösa detta att använda bibliotek som `Text.Regex` och `split`. 

Ett alternativ till de inbyggda funktionerna är att använda listfunktioner som `filter` och `map` med hjälp av list comprehension.

En annan detalj är att `take`, `drop` och `slice` intern realiserar substrängsextraktion genom kopiering av teckensekvenser. Detta kan leda till viss prestandadebitering vid stora datamängder.

## Se även

Herald programming blog: [Extracting substrings in Haskell](https://heraldprogramming.com/2021/12/extracting-substrings-in-haskell)

School of Haskell: [Substrings in Haskell - How to Do it Efficiently](https://www.schoolofhaskell.com/user/peter/efficient-substrings)