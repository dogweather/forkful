---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Kotlin: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

Kotlin ja säännölliset lausekkeet - mitä ne ovat ja miksi ohjelmoijat niitä käyttävät?

## What & Why?

Säännölliset lausekkeet ovat erityinen merkkijonojen käsittelytekniikka, joka auttaa tarkistamaan ja löytämään tiettyjä merkkijonoja tai malleja tekstissä. Ohjelmoijat käyttävät säännöllisiä lausekkeita useisiin tarkoituksiin, kuten tekstien analysointiin, tiedon hakemiseen ja manipulointiin.

## How to:

Käyttämällä säännöllisiä lausekkeita Kotlin-ohjelmoinnissa, voit ensin luoda säännöllinen lauseke-luokan käyttämällä Regex() -funktiota ja antamalla sille haluamasi lausekkeen. Sitten voit käyttää erilaisia metodeja, kuten find(), replace() ja match(), hakeaksesi tietoa ja manipuloidaksesi merkkijonoja.

Esimerkki 1: Hae numerot listasta
```Kotlin
val text = "Tämä on lista numeroista: 1, 2, 3 ja 4"
val regex = Regex("\\d+")
val matches = regex.findAll(text)
matches.forEach { println(it.value) }
```
Tulostus:
```
1
2
3
4
```

Esimerkki 2: Vaihda sanat listassa
```Kotlin
val text = "Tämä on lista sanoista: kissa, koira, lintu ja kala"
val regex = Regex("\\w+")
val result = regex.replace(text) { match ->
    when (match.value) {
        "kissa" -> "hevonen"
        "koira" -> "leijona"
        "lintu" -> "papukaija"
        "kala"  -> "valas"
        else -> match.value
    }
}
println(result)
```
Tulostus:
```
Tämä on lista sanoista: hevonen, leijona, papukaija ja valas
```

## Deep Dive:

Säännölliset lausekkeet ovat olleet käytössä jo yli 60 vuotta ja niitä käytetään laajasti eri ohjelmointikielissä. Ne voivat olla hyödyllisiä monissa projekteissa, mutta on myös vaihtoehtoja, kuten string-metodeja ja lambda-funktioita, joiden avulla voidaan saavuttaa samat tavoitteet. Kotlinissa säännölliset lausekkeet on toteutettu Java-kielen Regex-luokan avulla, joten sinun ei tarvitse opetella uutta syntaksia.

## See Also:

Voit lukea lisää Kotlinin säännöllisistä lausekkeista Kotlin-julkaisukirjasta ja Kotlinin virallisilta verkkosivuilta. Voit myös tutustua Java-kielen Regex-luokan dokumentaatioon, jotta ymmärrät paremmin sen käyttöä Kotlinissa.