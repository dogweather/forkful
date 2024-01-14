---
title:    "Swift: Schriftlicher Standardfehler"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Schreiben an die Standardfehlermeldung (Standard Error) kann hilfreich sein, um Fehler in Code zu identifizieren und das Debuggen zu erleichtern. Es ist auch eine Möglichkeit, um zusätzliche Informationen während der Programmausführung zu erhalten.

## Wie man schreibt
Um etwas an die Standardfehlermeldung zu schreiben, können Sie die Funktion `print()` mit dem Argument `to` und dem Wert `stderr` verwenden. Hier ist ein Beispiel in Swift:

```Swift
print("Dies ist ein Beispiel für die Standardfehlermeldung.", to: &stderr)
```

Das Ergebnis sieht dann folgendermaßen aus:

```
Dies ist ein Beispiel für die Standardfehlermeldung.
```

## Tiefenblick
Es ist wichtig zu beachten, dass das Schreiben an die Standardfehlermeldung nur innerhalb einer laufenden Anwendung funktioniert und nicht in einer Xcode-Konsole oder im Terminal. Außerdem ist es wichtig, sicherzustellen, dass das Logging-Level richtig eingestellt ist, damit die Ausgabe auch tatsächlich auf der Standardfehlermeldung erscheint.

## Siehe auch
- [Offizielle Dokumentation zu Standard Error in Swift](https://developer.apple.com/documentation/swift/standarderror)
- [Schreiben in die Standardfehlermeldung mit der Funktion `fprintf()`](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output.html)
- [Diskussion über die Verwendung von Standard Output und Standard Error in Swift](https://forums.swift.org/t/standard-error-stream-unthread-safe/14532)