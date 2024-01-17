---
title:                "Interpolieren einer Zeichenkette"
html_title:           "Elm: Interpolieren einer Zeichenkette"
simple_title:         "Interpolieren einer Zeichenkette"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Einfügen von Variablen oder Ausdrücken in einen String, auch bekannt als String-Interpolation, ist ein häufiger Bestandteil der Programmierung. Dieser Prozess ermöglicht es Entwicklern, dynamische Inhalte in statische Texte zu integrieren, was die Lesbarkeit und Wartbarkeit des Codes verbessert.

## Wie geht's?
Im Elm können wir die String-Interpolation einfach mit dem '$'-Zeichen und geschweiften Klammern durchführen:

```Elm
let name = "Max"
let age = 25
let output = "Hallo, ich heiße ${name} und bin ${age} Jahre alt"
```

Wir geben somit zwei Variablen in einen Textstring ein und erhalten als Output: "Hallo, ich heiße Max und bin 25 Jahre alt". Wir können auch Funktionen oder Operationen in die geschweiften Klammern einfügen, um komplexe Strings zu erstellen.

## Tiefer Einblick
String-Interpolation ist ein einfaches und effektives Werkzeug, um Strings mit dynamischen Inhalten zu erstellen. Besonders in Verbindung mit internationalen Texten oder HTML-Elementen kann dies enorm hilfreich sein.

Es gibt auch alternative Methoden, wie z.B. die Verwendung von String-Konkatenation oder die Verwendung von Formatierungsoperatoren. Jedoch kann dies zu unübersichtlichem Code und möglicherweise zu Fehlern führen.

Bei der Implementierung der String-Interpolation in Elm wird der Textparser-Algorithmus verwendet, um die Variablen oder Funktionen innerhalb der geschweiften Klammern zu identifizieren und mit den dazugehörigen Werten zu ersetzen.

## Siehe auch
Weitere Informationen zu String-Interpolation in Elm finden Sie in der offiziellen Dokumentation unter: https://guide.elm-lang.org/strings/interpolation.html