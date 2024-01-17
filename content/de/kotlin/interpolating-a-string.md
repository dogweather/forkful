---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Kotlin: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Stringinterpolation ist eine Möglichkeit, dynamische Werte in einem String einzufügen, anstatt mehrere Strings zusammenzufügen. Programmierer nutzen es, um Code lesbarer, kürzer und effizienter zu gestalten.

## Wie funktioniert es?
## Wie es geht:
Das Interpolieren von Strings in Kotlin ist einfach. Benutze einfach ein Dollarzeichen zusammen mit dem Variablennamen innerhalb eines Strings und der Wert der Variablen wird automatisch eingefügt. Hier ist ein Beispiel:
```
val name = "Mark"
val message = "Hallo, mein Name ist $name"
print (message)
```
Dies wird die Ausgabe "Hallo, mein Name ist Mark" erzeugen.

## Tiefer Einblick
Stringinterpolation wurde in der Programmiersprache Perl eingeführt und ist seitdem in vielen anderen Sprachen wie Kotlin, Swift und Python übernommen worden. Eine Alternative zum Interpolieren von Strings ist die Verwendung der Methode "format" in Java. Die Implementierung von Stringinterpolation in Kotlin basiert auf der Verwendung von Templates und ist in der Compiler-Ebene integriert, um die Runtime-Effizienz zu verbessern.

## Siehe auch
Weitere Informationen zum Interpolieren von Strings in Kotlin findest du auf der offiziellen Kotlin-Website: https://kotlinlang.org/docs/reference/basic-types.html#string-templates