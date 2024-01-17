---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Fish Shell: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Miksi convertata merkkijono pieniksi kirjaimiksi? Tämä on yleinen toimenpide ohjelmoinnissa, kun halutaan varmistaa, että syötetty merkkijono vastaa tiettyä standardeja tai vertailu tapahtuu oikein. Pienillä kirjaimilla varmistetaan myös yhdenmukainen muotoilu.

## Miten tehdä:

Fish Shell tarjoaa helpon tavan muuttaa merkkijonon pieniksi kirjaimiksi. Tämä tapahtuu käyttämällä `string tolower` komentoa ja antamalla haluttu merkkijono argumenttina.

```
Fish Shell> string tolower "HELSINKI"
helsinki
```

Mikäli halutaan tallentaa pienet kirjaimet muuttujaan, komennolla voi käyttää myös `-o` flagia, joka asettaa muutetun merkkijonon muuttujaan.

```
Fish Shell> set city "HELSINKI"
Fish Shell> string tolower -o city
Fish Shell> echo $city
helsinki
```

## Syvempi sukellus:

Historiallisesti, merkkijonon konverttaaminen pieniksi kirjaimiksi on ollut tarpeellinen osa tietokoneohjelmointia, sillä eri käyttöjärjelmät käsittävät kirjaimia eri tavoin. Esimerkiksi IBM mainframe tietokoneet käyttävät isoja kirjaimia, kun taas UNIX pohjaiset järjestelmät käyttävät pieniä kirjaimia. Fish Shellin toiminto perustuu käyttöjärjestelmän `tr` komentoon, joka suoritetaan taustalla.

Mikäli Fish Shell ei ole käytettävissä, vastaavan komennon voi suorittaa myös Bash Shellissä käyttämällä `tr` komentoa ja `[:upper:]` ja `[:lower]` merkkisarjoja.

## Katso myös:

-  Fish Shellin virallinen dokumentaatio: https://fishshell.com/docs/current/cmds/string.html
-  `tr` komennon dokumentaatio: https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/stroupperlower.htm
-  `tr` komennon julkaisuhistoria: https://manpages.debian.org/testing/coreutils/tr.1.en.html