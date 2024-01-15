---
title:                "Tilapäistiedoston luominen"
html_title:           "Bash: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi saattaisit haluta luoda väliaikaisen tiedoston Bash-skriptissä. Yleisimmät syyt ovat tietojen tallentaminen väliaikaisesti sekä tiedostojen prosessointi ja manipulointi.

## Näin
Bash-tehtävän suorittamisen aikana saatat tarvita väliaikaista tiedostoa tallentaaksesi väliaikaisesti muuttuvia tietoja tai prosessoitaaksesi tiedostoja. Tämä onnistuu luomalla väliaikainen tiedosto `mktemp` komennolla.

```Bash
tmpfile=$(mktemp) # tiedostonimi tallennetaan muuttujaan
echo "Tämä on väliaikainen tiedosto" > "$tmpfile"
cat "$tmpfile" # tulostetaan tiedoston sisältö
```

Suoritettaessa tätä skriptiä, näet seuraavan tulosteen:

```
Tämä on väliaikainen tiedosto
```

## Syvempi sukellus
`mktemp` komento luo uniikin väliaikaisen tiedoston, jonka nimi tallennetaan muuttujaan. Oletuksena tiedosto luodaan `/tmp` hakemistoon, mutta voit määrittää haluamasi hakemiston `--tmpdir` argumentilla.

Voit myös itse määrittää väliaikaisen tiedoston nimen `–t` argumentilla. Tämä on hyödyllistä, jos haluat luoda useita väliaikaisia tiedostoja ja pitää kirjaa niiden nimistä.

Lisäksi voit määrittää tiedostomaskin `–m` argumentilla, joka sallii sinun asettaa tiedoston oikeudet luomisen yhteydessä.

```Bash
mktemp -m 666 --tmpdir=/home/käyttäjä/Dokumentit –t tmp-XXXXXX
```

Tämä komento luo väliaikaisen tiedoston nimeltään `tmp-XXXXXX`, jonka oikeudet ovat 666 ja sijainti `/home/käyttäjä/Dokumentit` hakemistossa.

## Katso myös
- `man mktemp` komentorivin käyttöohjeet
- [Linux Bash -opas](https://linux.die.net/man/1/mktemp) `mktemp` komennosta
- [Bash skriptaus tutoriaali](https://linuxize.com/post/bash-scripting-tutorial/) johon sisältyy myös väliaikaisten tiedostojen luominen