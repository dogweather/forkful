---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Suche und Ersetzung von Text ist eine rechenintensive Operation, mit der ein bestimmtes Muster in einer Zeichenkette identifiziert und durch ein anderes ersetzt wird. Dies ist bei der Manipulation von Text oder Daten nützlich, wie beispielsweise zur Änderung von Konfigurationsparametern in Quelldaten.

## Wie geht's:
Die `replace()` Methode der `String` Klasse kann zum Suchen und Ersetzen von Text in Arduino verwendet werden. Hier ist ein einfaches Beispiel, das zeigt, wie es funktioniert:

```Arduino
String myString = "Hallo, meine Freunde!";
myString.replace("Hallo", "Tschüß");
Serial.print(myString); // Gibt "Tschüß, meine Freunde!" aus
```

In diesem Beispiel suchen wir den Text "Hallo" in `myString` und ersetzen ihn durch "Tschüß". 

## Vertiefung
Die `replace()` Methode ist eine relative Neuheit in der Arduino-Entwicklung. In früheren Versionen mussten Such- und Ersetzungsoperationen manuell programmiert werden, meist mit Hilfe von Zeigern und Schleifen.

Ein alternatives Verfahren zur `replace()` Methode ist die Verwendung der `strstr()` Funktion, einer Standard-C Funktion, die einen Zeiger auf den ersten Vorkommnispunkt einer Substring in einer Zeichenkette zurückgibt. Dies kann mit anderen C-Funktionen kombiniert werden, um eine Such- und Ersetzungsoperation durchzuführen, erfordert jedoch mehr Programmieraufwand.

Was Implementierungsdetails betrifft, so ersetzt `replace()` tatsächlich den Originalstring und gibt kein neues Stringobjekt zurück. Dies ist etwas, das man im Hinterkopf behalten sollte, besonders wenn man mit großen Mengen von Daten arbeitet, da dies die Leistung beeinträchtigen kann.

## Weiteres nachschlagen

- Weitere Informationen zur `replace()` Methode finden Sie in der [Arduino-Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/).
- Für sehr große Textmengen, empfiehlt sich die Verwendung eines spezielleren Tools wie [grep](https://www.gnu.org/software/grep/), das auf effizientere Textsuch- und Ersatzmethoden zurückgreift.