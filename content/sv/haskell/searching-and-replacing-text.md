---
title:                "Haskell: Sökning och ersättning av text"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig färdighet för programmerare, oavsett vilket språk de använder. Detta kan till exempel vara användbart när man vill hitta och ändra specifika delar av koden eller när man vill uppdatera stora mängder data på en gång.

## Hur man gör det

För att söka och ersätta text i Haskell används funktionen `substitute`, som finns inbyggd i `Text`-modulen. Den tar tre argument: det mönster man vill söka efter, det man vill ersätta det med och den text som man vill söka i. Detta kan se ut såhär:

```
Haskell
import Data.Text

-- Sök och ersätt "world" med "universe" i strängen "Hello world!"
substitute "world" "universe" "Hello world!"
-- Output: "Hello universe!"
```

Man kan även använda reguljära uttryck i mönstret, genom att använda funktionen `regex` från `Text.Regex`-modulen:

```
Haskell
import Data.Text.Regex

-- Sök och ersätt alla förekomster av siffror i en sträng med "*"
substitute (regex "[0-9]+") "*" "This is a string with 1 and 2"
-- Output: "This is a string with * and *"
```

Det är även möjligt att använda funktionen `replaceAll` för att söka och ersätta i alla förekomster av en viss text i en sträng, istället för bara den första. Detta kan göras på följande sätt:

```
Haskell
import Data.Text

-- Ersätt alla "hello" med "hi" i strängen "hello hello hello"
replaceAll "hello" "hi" "hello hello hello"
-- Output: "hi hi hi"
```

## Djupdykning

För de som vill lära sig mer om hur man effektivt kan söka och ersätta text i Haskell så finns det flera olika moduler och funktioner som kan vara till hjälp. Till exempel är `Text.Regex`-modulen väldigt kraftfull när det kommer till att använda reguljära uttryck. Det finns även andra externa moduler som kan vara användbara, som till exempel `Text.Search` och `Text.Regex.TDFA`.

En annan viktig punkt att tänka på när det kommer till att söka och ersätta text är prestanda. Om man arbetar med stora mängder data och behöver söka och ersätta i flera olika strängar, kan det vara värt att undersöka vilken modul eller funktion som ger bäst resultat för ens specifika användning.

## Se även

- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html
- https://hackage.haskell.org/package/text-regex-1.0.0.3/docs/Data-Text-Regex.html
- https://hackage.haskell.org/package/text-search-0.1.0.0/docs/Data-Text-Search.html