---
title:                "Tekstitiedoston lukeminen"
html_title:           "Gleam: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tiedostojen lukeminen ohjelmoinnissa tarkoittaa tekstin lukemista tiedostosta ja sen käsittelyä ohjelmassa. Tätä hyödynnetään usein ohjelmissa, jotka tarvitsevat tietoa ulkopuolisista lähteistä, kuten tiedostoista tai tietokannoista.

## Miten:
### Avaus
```
Gleam.IO.File.open("tekstitiedosto.txt", \get_contents)
__debug__
```
### Esimerkki
```
person = [name("Matti"), age(32), occupation("Developer")]

person = [name("Liisa"), age(24), occupation("Designer")] gleam_sys.IO.File.open("henkilötiedot.txt", \get_contents)
__debug__
```
### Tuotanto
```
person = [name("Matti"), age(32), occupation("Developer")]
person = [name("Liisa"), age(24), occupation("Designer")]
```

## Syväsukellus
Tiedostojen lukemisella on pitkä historia ohjelmoinnissa ja se on vakiinnuttanut paikkansa ohjelmointikielten perustoimintojen joukossa. On myös olemassa muita tapoja lukea tekstitiedostoja, kuten tiedostojen lukeminen binääritietoina tai käyttämällä erikoisempia kirjastoja, kuten Regular Expressions.

## Katso myös:
- [Gleam dokumentaatio](https://gleam.run)
- [Tekstitiedostojen lukeminen Pythonilla](https://realpython.com/read-write-files-python/)
- [Hex luokan hyödyntäminen tekstitiedostojen lukemisessa Java:ssa](https://www.baeldung.com/java-hex-class-reading-file)