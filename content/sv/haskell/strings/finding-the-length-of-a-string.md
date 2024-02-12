---
title:                "Hitta längden på en sträng"
aliases:
- /sv/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:39.979624-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör detta för att hantera och validera textdata effektivt.

## Så här gör du:
Haskell är enkelt. Använd `length` funktionen på en sträng för att få dess längd. Här är ett exempel:

```haskell
main :: IO ()
main = print (length "Hej Sverige!")
```

Exempel på output:

```
12
```

## Fördjupning
`length` funktionen i Haskell har sina rötter i funktionell programmering, där operationer på listor är grundläggande. Strängar hanteras som listor av tecken och `length` ger helt enkelt antalet element.

Det finns alternativ till `length`. Du kan skriva en rekursiv funktion för att räkna tecken eller använda `foldr` för att ackumulera längden. Men `length` är standard och effektiv.

Implementationen av `length` är optimerad men får kritik för att vara långsam med mycket stora listor, då den måste gå igenom varje element.

## Se även
- [Haskell Language Documentation](https://www.haskell.org/documentation/)
- ["Learn You a Haskell for Great Good!" av Miran Lipovača](http://learnyouahaskell.com/)
