---
title:                "Merkkijonon muuttaminen pienaakkosiksi"
html_title:           "Kotlin: Merkkijonon muuttaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Miksi me koodarit muuttelemme merkkijonoja? Sehän kuulostaa melkein kuin jokapäiväiseen läträämiseen sillä. No, totta puhuen me kuitenkin teemme sitä usein. Ja miksi? Koska usein meidän täytyy verrata kahta merkkijonoa keskenään tai vertaamme käyttäjän syöttämää merkkijonoa ja odotettua vastausta. Ja yhteinen työkalu, jolla voimme helposti vertailla merkkijonoja on niiden muuttaminen pieniksi kirjaimiksi.

## Kuinka:

Kuinka siis muutamme merkkijonon pieniksi kirjaimiksi ```Kotlin .toLowerCase()``` metodilla? Katso esimerkki:

```
val s = "Hei, Minä Olen String!"
println(s.toLowerCase())
```

Tämä koodi tulostaa ```hei, minä olen string!```. Huomaa, että alkuperäistä merkkijonoa ei muuteta, vaan uusi merkkijono luodaan.

## Syvempi sukellus:

Miksi Python ja muut kielet käyttävät ```Python .lower()``` ja Java käyttää ```Java .toLowerCase()``` metodeja? Onko niillä eroa? Kyllä, niillä on pieniä eroja. Kotlinin ```to LowerCase()``` metodi käyttää Unicode-standardia, jossa kirjain "I" muuttuu "i":ksi, mutta Pythonin metodi ei tee tätä muutosta. Tämä voi aiheuttaa ongelmia, jos esimerkiksi vertailet tietokantojen merkkijonoja, jotka on tallennettu eri kielissä.

Halutessasi voit myös käyttää ```Kotlin .toLowerCase(Locale)``` metodia, jotta voit määrittää, millä alueella olet, jolloin se käyttää kyseisen alueen kirjainmuutoksia.

## Lue myös:

Lisätietoa merkkijonojen käsittelystä Kotlinissa: https://kotlinlang.org/docs/reference/strings.html

Kotlinin Unicode-standardista: https://kotlinlang.org/docs/reference/strings.html#unicode-support