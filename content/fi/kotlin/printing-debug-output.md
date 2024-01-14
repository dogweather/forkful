---
title:                "Kotlin: Virheenkorjaustulosteiden tulostaminen"
simple_title:         "Virheenkorjaustulosteiden tulostaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi joku saattaisi haluta tulostaa debuggaustietoja ohjelmassa. Yksi yleisimmistä ongelmanratkaisutarkoituksessa on löytää virheitä tai vikoja koodista. Tulostamalla debuggaustietoja, voidaan seurata koodin suoritusta ja selvittää missä vaiheessa virheilmoitus tulee.

## Miten

Kotlinilla on helppo tulostaa debuggaustietoja ohjelmassa. Tähän käytetään `print()` tai `println()` funktiota, jotka tulostavat halutun arvon konsoliin. Alla on esimerkkejä erilaisista tulostusmahdollisuuksista Kotlinilla:

```Kotlin
val nimi = "Matti"
val ikä = 35

print(nimi) // Tulostaa "Matti"
println(nimi + ikä) // Tulostaa "Matti35"
println("Nimi: $nimi, Ikä: $ikä") // Tulostaa "Nimi: Matti, Ikä: 35"
```

Kuten nähdään esimerkeissä, `println()` tulostaa halutun arvon ja siirtyy seuraavalle riville, kun taas `print()` vain tulostaa arvon ilman rivinvaihtoa. Lisäksi voidaan käyttää `'` merkkejä tulostettavan arvon ympärillä, jolloin arvo tulostetaan sellaisenaan ilman tarvetta `+` merkille.

## Syväsyvennys

Debuggaustietoja tulostetaan myös usein tutkittaessa ohjelman suorituskykyä ja sen aikana tapahtuvia muutoksia muuttujissa. Tämä helpottaa ohjelman optimointia ja parantaa sen suorituskykyä. Lisäksi tulostetut tiedot voivat olla hyödyllisiä myös ohjelman dokumentoinnissa ja ongelmanratkaisussa.

## Katso myös

- https://kotlinlang.org/docs/tutorials/command-line.html
- https://kotlinlang.org/docs/reference/basic-syntax.html#using-string-templates
- https://www.jetbrains.com/help/idea/debugging-with-variables.html