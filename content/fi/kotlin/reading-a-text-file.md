---
title:                "Kotlin: Lukeminen tekstitiedostosta"
simple_title:         "Lukeminen tekstitiedostosta"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi
Tämän artikkelin tarkoituksena on auttaa sinua ymmärtämään kuinka lukea tekstitiedostoja Kotlinin avulla. Tekstitiedostot ovat yleisiä formaatteja tiedon tallentamiseen, ja on hyödyllistä tietää kuinka lukea niitä ohjelmallisesti.

## Kuinka lukea tekstitiedostoja Kotlinilla
Kotlinilla voi lukea tekstitiedostoja helposti käyttämällä "File" -luokkaa ja sen tarjoamia metodeja. Alla on esimerkki kuinka luodaan "File" -olio ja lukea sen sisältö:

```Kotlin
val tiedosto = File("tekstitiedosto.txt")
val sisalto = tiedosto.readText()
```

Koodin ensimmäisellä rivillä luodaan "File" -olio, jolla on parametrina tiedoston nimi. Toisella rivillä tiedoston sisältö luetaan muuttujaan käyttäen "readText()" -metodia. Tämän jälkeen voit käsitellä tiedoston sisältöä haluamallasi tavalla.

## Syvemmät tiedot tekstitiedostojen lukemisesta
Tekstitiedostojen lukeminen voi aiheuttaa haasteita, jos tiedoston sisältö ei ole muotoiltu sopivaksi. Esimerkiksi jos tiedostossa on paljon dataa, yksi keino helpottaa sen käsittelyä on käyttää "BufferedReader" -luokkaa. Tämä luokka mahdollistaa tiedoston lukemisen rivi kerrallaan, mikä voi olla hyödyllistä esimerkiksi jos haluat käsitellä jokaista riviä erikseen.

Toinen tärkeä asia huomioitavaksi on tiedoston sijainti ja nimi. Jos haluat lukea tiedoston jostain muualta kuin nykyisestä hakemistosta, sinun on annettava koko polku tiedostoon. Esimerkiksi Windowsilla polku voisi näyttää tältä: "C:\Users\käyttäjänimi\tekstitiedosto.txt". Jos taas haluat lukea tiedoston resurssikansiosta, voit käyttää "getResource()" -metodia.

## Katso myös
- [Kotlindokumentaatio](https://kotlinlang.org/docs/reference/)
- [Kotlinsäiebibliotekin "File" -luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/kotlin.io.-file/)

Kiitos kun jaoit aikaa tämän artikkelin lukemiseen ja toivottavasti siitä oli sinulle hyötyä!