---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Kotlin: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessamme ohjelmia, on usein tarpeen tallentaa tai lukea tietoa tekstitiedostosta. Tekstitiedostoja voidaan käyttää esimerkiksi tietokantojen korvikkeena tai tallentamaan käyttäjän syöttämiä tietoja. Kotlinilla tekstitiedoston kirjoittaminen on helppoa ja tehokasta.

## Kuinka tehdä se

Kirjoittaaksesi tekstitiedoston Kotlinilla, sinun tulee ensin luoda tiedosto ja siihen liittyvä BufferedWriter-olio. Seuraavaksi voit kirjoittaa haluamasi tiedot käyttäen BufferedWriterin write-metodia ja lopuksi sulkea tiedosto close-metodin avulla.

```Kotlin
val tiedosto = File("tiedosto.txt")
val writer = BufferedWriter(FileWriter(tiedosto))

writer.write("Tämä on esimerkki tekstitiedoston kirjoittamisesta Kotlinilla.")
writer.write("Voit lisätä haluamiasi tekstejä ja rivinvaihtoja.")

writer.close()
```

Tämän jälkeen voit avata tiedoston ja nähdä siellä kirjoittamasi tekstit. Huomaa, että jos tiedostoa ei löydy, se luodaan automaattisesti.

```
Tämä on esimerkki tekstitiedoston kirjoittamisesta Kotlinilla.
Voit lisätä haluamiasi tekstejä ja rivinvaihtoja.
```

## Syvennä tietämystäsi

Tekstitiedoston kirjoittaminen Kotlinilla käyttäen BufferedWriteria on tehokas tapa tallentaa tietoa, mutta on myös tärkeää ymmärtää, että tiedoston kirjoittaminen on I/O-toiminto, joka voi hidastaa ohjelman suorituskykyä. Tästä syystä on suositeltavaa käyttää esimerkiksi Thread-luokan metodeja kirjoittaessa tekstitiedostoa suorituksen hitaammille alueille.

## Katso myös

- [Kotlinin viralliset dokumentit](https://kotlinlang.org/docs/tutorials/kotlin-for-py/creating-classes.html)
- [Java I/O-opetusohjelma](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-writer-writer-writer.html)