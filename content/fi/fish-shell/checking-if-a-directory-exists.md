---
title:                "Tarkista, onko kansio olemassa"
html_title:           "Fish Shell: Tarkista, onko kansio olemassa"
simple_title:         "Tarkista, onko kansio olemassa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointityössä tarvitsemme tarkistaa, onko tietty kansio olemassa ennen kuin voimme suorittaa tiettyjä toimintoja. Fish Shellissä on muutamia tapoja tarkistaa tämä ja tässä artikkelissa näytämme, miten se tehdään.

## Miten tehdä

Fish Shell tarjoaa kaksi tapaa tarkistaa, onko kansio olemassa: käyttämällä `test` -komentoa tai käyttämällä `if` -lauseketta.

### Käyttäen `test` -komentoa

```Fish Shell
test -d [kansion nimi]
```
Tämä komento palauttaa arvon `0` jos kansio on olemassa ja `1` jos se ei ole.

### Käyttäen `if` -lauseketta

```Fish Shell
if test -d [kansion nimi]
    echo "Kansio on olemassa!"
end
```
Käyttämällä `if` -lauseketta, voimme suorittaa toimintoja vain jos kansio on olemassa.

## Syvällinen tieto

Fish Shellin `test` -komennolla on myös muita vaihtoehtoja, kuten tarkistaa onko kyseessä symbolinen linkki tai onko kansio tyhjä. Voit lukea lisää näistä vaihtoehdoista [Fish Shellin dokumentaatiosta](https://fishshell.com/docs/current/cmds/test.html).

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Kansioon siirtyminen Fish Shellissä](https://github.com/joonaprojects/Turbo-Code-Tasks/blob/master/Fish-Shell/fint_description.md#kansioon-siirtyminen)