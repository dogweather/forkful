---
title:    "Haskell: Att hitta längden på en sträng."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kan vara användbart när man arbetar med text i Haskell. Det kan hjälpa till att bestämma storleken på en sträng eller att kontrollera om en sträng är tom.

## Så här gör du

För att hitta längden på en sträng i Haskell kan du använda funktionen `length`. Den accepterar en sträng som argument och returnerar längden på strängen som en heltalsvärd.

```Haskell
-- Exempel på att hitta längden på en sträng
length "Hej hej!" -- output: 8
length "Jag älskar Haskell" -- output: 17
```

Funktionen `length` är inbyggd i Haskell, så du behöver inte importera några externa bibliotek för att använda den.

## Djupdykning

Om du vill utforska längdberäkningen ytterligare kan du titta på dess implementering i Haskell. Funktionen `length` är rekursiv och går igenom varje tecken i strängen tills den når slutet. Detta beräknar sedan längden genom att lägga till 1 för varje tecken i strängen.

```Haskell
-- Implementation av length funktionen
length :: [a] -> Int
length [] = 0 -- basfall, en tom sträng har längden 0
length (x:xs) = 1 + length xs -- rekursivt kall, lägger till 1 för varje tecken i strängen
```

## Se även

- [Haskell för nybörjare (på svenska)](https://www.haskell.org/svenska.html)
- [Officiell Haskell-dokumentation](https://www.haskell.org/documentation/)
- [Rekursion i Haskell](https://wiki.haskell.org/Recursion)