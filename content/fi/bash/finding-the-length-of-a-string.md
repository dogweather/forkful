---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Bash: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pituuden löytäminen on yksinkertainen toimenpide, jossa ohjelma laskee merkkijonossa olevien merkkien määrän. Tämä voi olla hyödyllistä esimerkiksi merkkijonojen vertailussa tai tiedonkäsittelyssä. 

Ohjelmointimaailmassa merkkijonojen pituuden laskeminen on hyvin yleistä ja hyödyllistä. Tämä auttaa ohjelmoijia tekemään tarkempia vertailuja tai valitsemaan oikeat toimenpiteet tarvittavalle tiedolle.

## Miten:

Koodi on yksinkertainen ja käyttää `expr`-komentoa laskeakseen merkkijonon pituuden:

```Bash
len=$(expr length "Tämä on merkkijono")
echo "Merkkijonon pituus on $len"
```

Tuloksena saadaan "Merkkijonon pituus on 20", sillä merkkijonossa on 20 merkkiä. 

## Syvemmälle:

Merkkijonon pituuden laskeminen on ollut osa ohjelmoinnin perustoimintoja pitkään. Aikaisemmin käytettiin erilaisia menetelmiä, kuten `wc -c` tai `awk '{print length}'`. Nykypäivänä `expr`-komento on luotettavampi ja tehokkaampi vaihtoehto.

On myös muita tapoja laskea merkkijonon pituus, kuten käyttää `$(#variable)` -rakennetta tai `expr`-komentoa yhdessä `awk`-komenton kanssa. Näitä vaihtoehtoja kannattaa kokeilla ja valita itselleen parhaiten sopiva tapa.

## Katso myös:

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Stack Overflow - How to find length of a string in Bash](https://stackoverflow.com/questions/17368067/how-to-find-length-of-a-string-in-bash)