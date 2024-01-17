---
title:                "Sökning och ersättning av text"
html_title:           "Elm: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en vanlig uppgift inom programmering. Det innebär att hitta en specifik del av texten och sedan ersätta den med en annan. Det kan göras för att fixa buggar, ändra texten i en applikation eller för att skapa en ny version av koden.

## Så här gör du:

```Elm
-- söker efter alla förekomster av "hej" och ersätter dem med "tja"
text = "Hej världen! Hej Elm!"
newText = String.replace "hej" "tja" text
-- newText blir nu "Tja världen! Tja Elm!"
```

## Djupdykning:

Sökning och ersättning av text har funnits sedan programmeringens tidiga dagar och är en viktig del av utvecklingsprocessen. Det finns olika metoder för att utföra detta, som Regular Expressions eller olika inbyggda funktioner i programmeringsspråk. Elm har inbyggda funktioner för att söka och ersätta text med hjälp av String-modulen.

## Se även:

Om du vill lära dig mer om sökning och ersättning av text i Elm, rekommenderar vi att du tittar på följande länkar:

- [Officiell Elm dokumentation för sökning och ersättning](https://package.elm-lang.org/packages/elm-lang/core/1.0.0/String#replace)
- [En guide om att använda Regular Expressions i Elm](https://medium.com/front-end-weekly/how-to-use-regular-expressions-regex-in-elm-65917318881)
- [En jämförelse av olika sök- och ersättningsmetoder i olika programmeringsspråk](https://dev.to/awwsmm/finding-and-replacing-all-occurrences-in-multiple-files-using-vim-grep-sed-perl-and-hole-maybe-other-tools-too-4j4a)