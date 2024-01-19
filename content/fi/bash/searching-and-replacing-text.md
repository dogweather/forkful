---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsintä ja sen korvaaminen tarkoittaa esiintymien löytämistä merkkijonoista ja niiden korvaamista toisella merkkijonolla. Ohjelmoijat tekevät tämän tyypillisesti datan muokkaamiseksi tai virheiden korjaamiseksi.

## Näin se tehdään:

Seuraavassa näet miten voit etsiä ja korvata tekstiä käyttäen `sed` työkalua Bashissa.

```Bash
$ echo "Moi Maailma" | sed 's/Maailma/World/'
Moi World
```

Tässä esimerkissä `sed` etsii sanaa 'Maailma' ja korvaa sen sanalla 'World'.

## Syvällisemmin:

Etsintä ja korvaus on ollut osa UNIX-järjestelmiä vuosikymmenien ajan ja `sed` on vain yksi monista Unix-pohjaisista työkaluista, jotka tarjoavat tämän toiminnallisuuden. On olemassa monia muitakin työkaluja, kuten AWK ja Perl, mutta `sed` on usein suosittu sen yksinkertaisuuden vuoksi.

Vaikka Bash itse ei tarjoa suoraa tavalla etsiä ja korvata tekstiä, se tarjoaa kuitenkin mahdollisuuden hyödyntää näitä ulkoisia työkaluja tehdäkseen niin. Käyttäen näitä työkaluja, Bash-ohjelmoijat voivat muokata tiedostoja ja datavirtoja yksinkertaisilla ja tehokkailla tavoilla.

## Katso myös:

Jotta voisit oppia lisää, kannattaa tutustua seuraaviin lähteisiin:
- [GNU Sed - Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Using Bash's 'string operations' for substrings](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [AWK - A Tutorial by Example](https://www.grymoire.com/Unix/Awk.html)
- [Perl - Mother of Regex](https://www.perl.org/)