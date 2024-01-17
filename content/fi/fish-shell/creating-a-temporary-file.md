---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Fish Shell: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Luodaan ensin selvyyttä siihen, mitä on väliaikaisen tiedoston luominen. Tässä yhteydessä väliaikaisella tiedostolla tarkoitetaan tiedostoa, joka on luotu väliaikaisesti ohjelman suorituksen aikana, ja se poistetaan automaattisesti lopuksi. Ohjelmoijat tekevät tämän useimmiten silloin, kun he haluavat tallentaa väliaikaisia tietoja muistiin, mutta eivät halua pitää niitä pysyvästi.

## Kuinka tehdä?

Fish Shellissa väliaikaisen tiedoston luominen on helppoa. Käytä vain seuraavaa komentoa:

```
set tmpfile (mktemp)
```

Tämä luo väliaikaisen tiedoston nimeltä `$tmpfile`. Voit nyt tallentaa siihen haluamiasi tietoja ja käyttää niitä ohjelmassasi. Kun ohjelma on valmis, väliaikainen tiedosto poistetaan automaattisesti.

Voit myös luoda väliaikaisen tiedoston tietyssä kansiossa lisäämällä `--tmpdir` -vaihtoehdon, kuten esimerkissä:

```
set tmpfile (mktemp --tmpdir ~/Documents)
```

Tämä luo väliaikaisen tiedoston nimeltä `~/Documents/tmpfile`.

## Syvemmälle sukeltaminen

Historiallinen konteksti: Väliaikaiset tiedostot ovat olleet käytössä jo vuosia ja ovat vakiintunut tapa tallentaa väliaikaisia tietoja. Alkuperäinen UNIX-komentorivi-työkalu on nimeltään `mktemp`.

Vaihtoehtoja: On olemassa myös muita tapoja luoda väliaikaisia tiedostoja kuin `mktemp`, kuten `mkstemp` ja `mkdtemp`. Nämä antavat enemmän hallintaa luotujen tiedostojen nimille ja sijainneille, mutta ovat myös monimutkaisempia käyttää.

Toteutusyksityiskohdat: `mktemp` on käytännössä vain yksinkertainen Bash-scripti, joka käyttää `/dev/urandom` -lähde luodakseen uniikin tiedostonimen.

## Katso myös

[Käyttöohjeet Fish Shell-lle](https://fishshell.com/docs/current/)

[sinkronoi.io - Artikkeli Fish Shellistä](https://www.sinkronoi.io/article/fish-shell)

[Tietoa UNIX:ista ja mktemp-komennosta](https://en.wikipedia.org/wiki/Mktemp)