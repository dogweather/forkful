---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Kotlin: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Mallia vastaavien merkkien poistaminen on prosessi, jossa poistetaan merkkejä, jotka vastaavat tiettyä kaavaa. Ohjelmoijat tekevät tätä yleensä muuttaakseen tai puhdistaakseen merkkijonoja tietyn muodon tai sisällön saavuttamiseksi.

## Miten:
```
Kotlin val string = "Tämä on esimerkki lauseesta, jossa on paljon toistuvia merkkejä. Esimerkki esimerkki lauseesta, jossa on paljon toistuvia merkkejä."
val modifiedString = string.replace("esimerkki", "")
```
Tulostus:
```
"Tämä on lauseesta, jossa on paljon toistuvia merkkejä. Lauseesta, jossa on paljon toistuvia merkkejä."
```

## Syventävä tieto:
Mallia vastaavien merkkien poistaminen ei ole uusi asia ohjelmoinnissa. Ennen uudempia ohjelmointikieliä, kuten Kotlin, tämä oli usein tehtävä manuaalisesti käyttämällä muun muassa Regex-kaavoja. Nykyään Kotlinin mukana tulee kuitenkin monia käteviä toimintoja, kuten "replace()" -metodi, joka tekee prosessin paljon helpommaksi ja vähentää virheiden riskiä.

Vaihtoehtoisesti, jos haluat poistaa vain tietyn määrän toistuvia merkkejä, voit käyttää "replace()" -metodia yhdessä "replaceFirst()" -metodin kanssa, joka korvaa vain ensimmäisen esiintymän.

## Katso myös:
- [Regex](https://www.geeksforgeeks.org/kotlin-regular-expression/)
- [Kotlinin replace() -metodi](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)