---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon interpolointi on prosessi, jossa muuttujia tai ilmauksia lisätään suoraan merkkijonoihin. Ohjelmoijat tekevät sen koodin selkeyden ja ylläpidettävyyden parantamiseksi.

## Näin se tehdään:
Haskellissa voimme käyttää `Text.Printf` kirjastoa merkkijonon interpolointiin. Katsotaan esimerkkiä:

```Haskell
import Text.Printf (printf)

let x = 7
let y = 3
printf "Summa on: %d\n" (x + y)
```

Tämä antaa tulosteen: 

```Haskell
"Summa on: 10\n"
```

## Syvemmälle
Merkkijonon interpolointi on ratkaisu, joka löytyy useimmista moderneista ohjelmointikielistä. Haskellissa, alternatiivina `printf` funktiolle, voidaan käyttää `formatting` kirjastoa, joka tukee lazy ja strict-tyylisiä merkkijonoja. On hyvä huomioida, että vaikka merkkijonon interpolointi lisää koodin luku- ja ymmärrettävyyttä, se saattaa tehdä koodista hieman hitaamman suorituskyvyltään.

## Katso myös
Lisää koodiesimerkkejä ja merkkijonon interpolointia Haskellissa:
- [Haskell Printf](https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html)
- [Haskell Formatting](https://hackage.haskell.org/package/formatting)
- [Real World Haskell, Chapter 7: I/O](https://book.realworldhaskell.org/read/io.html)