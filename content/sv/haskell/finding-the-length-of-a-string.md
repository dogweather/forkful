---
title:                "Att hitta längden på en sträng"
html_title:           "Haskell: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är en vanlig uppgift inom programmering, som innebär att bestämma antalet tecken i en given sträng. Detta är användbart för att få information om t.ex. en text eller en datastruktur.

## Hur man gör:
För att hitta längden på en sträng i Haskell använder vi funktionen "length". Den tar en sträng som argument och returnerar längden som ett heltal. Här är ett exempel på kod och resultat:

```Haskell
length "Hej!" 
-- Output: 4 (eftersom det finns fyra tecken i "Hej!")
```

Du kan också använda denna funktion på en lista av tecken, även känd som en "string" i Haskell. Här är ett annat exempel:

```Haskell
length ['H','e','j'] 
-- Output: 3 (eftersom det finns tre tecken i ['H','e','j'])
```

## Djupdykning:
Att hitta längden på en sträng är en grundläggande uppgift som har funnits länge inom programmering. I många andra programmeringsspråk, som C eller Java, måste utvecklare skriva en loop för att räkna varje enskilt tecken i strängen. Men i Haskell är det enklare eftersom funktionen "length" redan är fördefinierad. En alternativ metod för att hitta längden av en sträng i Haskell är att använda funktionen "foldl", men det är mer avancerat och kommer inte att täckas här.

## Se även:
[Haskell String](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html) - Dokumentation för standardbiblioteket som innehåller funktionen "length" för att hitta längden av strängar i Haskell.