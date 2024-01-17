---
title:                "Merkkijonon pituuden etsiminen"
html_title:           "Kotlin: Merkkijonon pituuden etsiminen"
simple_title:         "Merkkijonon pituuden etsiminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Miksi stringin pituuden löytämistä tarvitaan ja mitä se tarkalleen ottaen on? Lausekkeessa "("Hello World").length" stringin "Hello World" pituus on 11 merkkiä, ja tätä tietoa voidaan käyttää erilaisten operaatioiden suorittamiseen ohjelmoinnissa.

## Kuinka:
```Kotlin 
// Esimerkki 1:
val string = "Tervetuloa maailma!"
println(string.length)
```
```Output:
20
```

```Kotlin 
// Esimerkki 2:
val string = "Tässä on 123"
println(string.length)
```
```Output:
13
```

```Kotlin 
// Esimerkki 3:
val string = ""
println(string.length)
```
```Output:
0
```

## Syvemmälle:
Stringien pituuden löytämistä on käytetty ohjelmoinnissa jo pitkään ja se on osa perusoperaatioita useimmissa ohjelmointikielissä. Tämä tieto on hyödyllistä esimerkiksi tekstipohjaisten pelien kehittämisessä tai käyttäjän syötteiden validoinnissa. Stringien pituuden löytämisen lisäksi ohjelmoijien on myös tärkeää ymmärtää erilaisia tapoja käsitellä ja muokata stringeja.

## Katso myös:
- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Java String Length](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [C++ String Length](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_length.htm)