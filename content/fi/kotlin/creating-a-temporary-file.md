---
title:                "Tilapäistiedoston luominen"
html_title:           "Kotlin: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Luodaan väliaikainen tiedosto, jotta ohjelmoijat voivat tallentaa ja käyttää hetkellisiä tietoja suorittaessaan ohjelmiaan. Näin he voivat varmistaa, että ohjelma toimii halutulla tavalla ja välttää lopullisten tiedostojen vahingoittumisen.

## Kuinka:
Esimerkiksi, voit luoda väliaikaisen tiedoston käyttämällä ```Kotlin File.createTempFile()``` metodia ja määrittämällä halutun tiedostonimen sekä tiedostopäätteen. Ohjelman suorittamisen jälkeen, väliaikainen tiedosto luodaan nykyisen työhakemiston alle.

## Syvempää tarkastelua:
Luontonhistorian osalta, väliaikaisia tiedostoja on käytetty jo pitkään ohjelmoinnissa. Alternatiivina, ohjelmoijat voivat myös käyttää väliaikaisia kansionijaksotiloja, mutta tämä ei ole niin kätevä ratkaisu kuin väliaikaisen tiedoston luominen. Tiedostojen luominen toimii Java.io.File-luokan avulla, joka tarjoaa myös muita tiedostoihin liittyviä toimintoja.

## Katso myös:
Kotlin offcial documentation: https://kotlinlang.org/docs/reference/file-input-output.html
Java.io.File class documentation: https://docs.oracle.com/javase/8/docs/api/java/io/File.html