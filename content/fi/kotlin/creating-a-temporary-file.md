---
title:    "Kotlin: Luodaan tilapäistiedosto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Joskus ohjelmoinnissa on tarve luoda väliaikainen tiedosto, joka poistetaan käytön jälkeen. Tämä voi olla hyödyllistä esimerkiksi testattaessa erilaisia tiedosto-operaatioita tai tallentaessa väliaikaista dataa. Kotlinilla tämä on helppoa ja kätevää.

## Miten luoda väliaikainen tiedosto Kotlinilla

Kotlinin standardikirjasto tarjoaa `createTempFile()` -funktion, jolla voidaan luoda väliaikainen tiedosto. Funktio ottaa parametreinaan halutun tiedostonimen alun sekä tiedoston päänimeen liitettävän pääteosan. Funktio luo tiedoston väliaikaiseen kansioon ja palauttaa tiedoston `File`-muodossa. Tämän jälkeen tiedostoa voi käyttää haluamallaan tavalla.

Esimerkiksi seuraava koodinpätkä luo väliaikaisen `.txt`-tiedoston nimeltä "testitiedosto" ja tulostaa tiedostonimen:
```Kotlin
val tempFile = createTempFile("testitiedosto", ".txt")
println(tempFile.name)
```
Tulostaa: `testitiedosto1201830472.txt`

## Syvemmälle väliaikaisten tiedostojen luomiseen

`createTempFile()` -funktion oletusarvot käytettävästä kansioista ja tiedostonimen alusta sekä pääteosasta voidaan määritellä myös parametreilla. Parametrit `directory` ja `prefix` ovat `String`-tyyppisiä ja `suffix` on `String?`-tyyppinen. `String?` viittaa `nullable`-tyyppiin, joka tarkoittaa, että parametri voi olla joko `not null` tai `null` arvo.

Lisäksi `deleteOnExit()` -funktiolla voidaan määritellä, poistetaanko väliaikainen tiedosto käytön jälkeen. Oletusarvoisesti tiedosto poistetaan.

Esimerkiksi seuraava koodinpätkä luo väliaikaisen `.json`-tiedoston nimeltä "testi" hakemistosta "tmp" ja poistaa sen käytön jälkeen:
```Kotlin
val tempFile = createTempFile("testi", ".json", File("tmp"), null)
tempFile.deleteOnExit()
```

## Katso myös

- [Kotlinin standardikirjasto - createTempFile](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Kotlinin standardikirjasto - File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)