---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Kotlin: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was ist Capitalizing und warum wird es von Programmierern genutzt?

Capitalizing ist der Prozess, bei dem der erste Buchstabe eines Strings in Großbuchstaben umgewandelt wird. Programmierer nutzen dies häufig, um Strings in einer konsistenten und lesbaren Form darzustellen, insbesondere bei der Darstellung von Benutzeroberflächen oder bei der Formatierung von Texten.

## So geht's:

```Kotlin
val name = "max mustermann"
println(name.capitalize())
```

Ausgabe: 
Max mustermann

## Tiefer Einblick:

Capitalizing wird oft als Teil des String-Formats angesehen und ist in vielen Programmiersprachen verfügbar. Es gibt jedoch auch alternative Methoden, um Strings in Großbuchstaben umzuwandeln, z.B. `toUpperCase()` oder `replace`.

Die Implementierung von `capitalize()` variiert je nach Programmiersprache, aber in der Regel wird der erste Buchstabe des Strings erkannt und in Großbuchstaben umgewandelt, während der Rest des Strings unverändert bleibt.

## Weitere Quellen:

- [Kotlin Dokumentation zu Strings](https://kotlinlang.org/docs/reference/whatsnew13.html#extended-jvm-library)
- [Alternative Methoden zum Formatieren von Strings](https://www.baeldung.com/java-string-capitalize)
- [GitHub Issue zum Thema `capitalize()` Implementierung in Kotlin](https://youtrack.jetbrains.com/issue/KT-10035)