---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Bash: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi ja miten:
Lukeminen komentoriviparametreista on tapa saada käyttäjältä syötön Bash-ohjelmalle. Tämä on hyödyllistä esimerkiksi jos haluat antaa ohjelmalle parametreja ajon yhteydessä, kuten tiedoston nimen tai käyttäjän nimen.

## Miten tehdä se:
Komentoriviparametrit luetaan Bash-skriptillä käyttämällä ```$1```, ```$2```, jne. Näiden avulla ohjelma saa tiedon järjestyksessä mikä parametri on annettu ensimmäisenä, toisena jne.

```Bash
#!/bin/bash
echo "Hei $1! Toivotamme sinut tervetulleeksi."
```
Tuloste:
```Bash
Hei Tom! Toivotamme sinut tervetulleeksi.
```

## Syvempi sukellus:
Komentoriviparametrit ovat olleet osa Unix-järjestelmiä jo vuodesta 1979. Yleisesti ne ovat käytössä komentorivisovelluksissa, kuten Bash. Bashin lisäksi myös muut ohjelmointikielet kuten C, Java ja Python tarjoavat mahdollisuuden lukea komentoriviparametreja. Lisäksi on olemassa myös muita tapoja antaa syötettä ohjelmille, kuten käyttöliittymät tai tiedostot.

## Katso myös:
- [Bash-skriptien perusteet](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Komentoriviparametrien luetteloiminen](https://www.cyberciti.biz/faq/unix-getopt-command-examples/)
- [Komentoriviparametrien ohjeet Bash-documentaatiossa](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)