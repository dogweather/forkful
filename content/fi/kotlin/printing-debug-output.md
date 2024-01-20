---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tulostaminen debug-tiedotuksena on koodin tuottaman tiedon jäljitystyökalu, joka tulostaa kyseisen tiedon konsoliin. Ohjelmoijat käyttävät sitä löytämään ja korjaamaan virheitä koodissa.

## Kuinka:

Tässä on esimerkki siitä, kuinka tulostetaan debug-tiedotus Kotlinilla:

```Kotlin
fun main() {
    val muuttuja = "Testi"
    println("Debug: $muuttuja")
}
```

Kyseisen ohjelman suorittaminen tuottaa seuraavan tulosteen:

```
Debug: Testi
```

## Syvällisemmin:

Tulostaminen debug-tiedotuksena on vanha ohjelmoinnin käytäntö ja se on yksi tehokkaimmista tavoista jäljittää ja korjata virheitä. On olemassa vaihtoehtoisia tapoja, kuten lokeihin kirjoittaminen tai erillisten debugger-työkalujen käyttö, mutta ne saattavat olla monimutkaista tai ne vaativat erillisen oppimisen. Debug-tulaostamisen toteutus yleensä liittyy järjestelmän standardiin ulostulotoimintoon (kuten `println` Kotlinissa), joka kirjoittaa tiedon konsoliin.

## Katso Myös:

1. [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/reference/)
2. [Debuggaus Kotlinissa](https://kotlinlang.org/docs/tutorials/debugging-with-intellij-idea.html)