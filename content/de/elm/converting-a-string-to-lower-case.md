---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Elm: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, wird oft die Funktion `String.toLower` verwendet. Programmierer verwenden dies, um sicherzustellen, dass die Eingabe unabhängig von der Groß- und Kleinschreibung korrekt verarbeitet wird. 

## Wie geht's:
```Elm
Name.toLower "ELM" == "elm"
```
Die Funktion `toLower` kann auf jede Zeichenkette angewendet werden und gibt die entsprechende Version in Kleinbuchstaben zurück. In diesem Beispiel wird "ELM" in "elm" umgewandelt.

```Elm
String.toLower "Lieber Leser!" == "lieber leser!"
```
Auch Umlaute und Sonderzeichen werden dabei korrekt umgewandelt.

## Tiefere Einblicke:
Diese Funktion existiert in vielen Programmiersprachen und wurde erstmals in der Sprache Lisp eingeführt. Sie wird oft verwendet, um Eingabedaten zu normalisieren und Fehler aufgrund von Groß- und Kleinschreibung zu vermeiden. Eine alternative Möglichkeit ist die Verwendung der Funktion `String.toCaseFold`, die zusätzlich auch sprachspezifische Konvertierungen durchführt.

## Siehe auch:
Weitere nützliche Funktionen für die Zeichenkettenumwandlung in Elm finden Sie in der offiziellen Dokumentation: https://package.elm-lang.org/packages/elm-lang/core/latest/String.