---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Elm: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Manchmal kann es nützlich sein, einen String in Kleinbuchstaben zu verwandeln. Zum Beispiel, wenn man überprüfen möchte, ob ein Benutzername bereits in einem System vergeben wurde. Die Konvertierung hilft auch dabei, Konsistenz bei der Verarbeitung von Eingabedaten zu gewährleisten.

## Wie man es macht
Um einen String in Kleinbuchstaben umzuwandeln, kann man in Elm die `String.toLower` Funktion verwenden. Sie akzeptiert einen String als Argument und gibt einen neuen String zurück, in dem alle Buchstaben in Kleinbuchstaben umgewandelt wurden.

```Elm
String.toLower "ELM ist eine funktionale Programmiersprache"
-- Ausgabe: "elm ist eine funktionale programmiersprache"
```

Wenn wir die Funktion auf einen String anwenden, der bereits klein geschrieben ist, bleibt der String unverändert.

```Elm
String.toLower "Elm ist toll"
-- Ausgabe: "elm ist toll"
```

## Tiefere Einblicke
Die `String.toLower` Funktion verwendet das Unicode-System, um Buchstaben in Kleinbuchstaben umzuwandeln. Dies bedeutet, dass auch nicht-englische Buchstaben korrekt konvertiert werden.

Es ist auch wichtig zu beachten, dass die `String.toLower` Funktion ein neues String-Objekt zurückgibt, anstatt den ursprünglichen String zu verändern. Dies ist wichtig, um die Unveränderlichkeit von Daten in Elm zu erhalten.

## Weitere Informationen
- [Elm Lang - Strings](https://guide.elm-lang.org/strings/)
- [Offizielle Elm Dokumentation - String Modul](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Blogbeitrag: Elm in 10 Minuten - Strings](https://dev.to/emilycate/elm-in-10-minutes-strings-1d3j)