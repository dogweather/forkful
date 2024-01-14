---
title:                "Swift: Schreiben zu Standardfehler"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Fehlermeldungen in der Swift-Programmierung ist eine wichtige Technik, um Fehler und Probleme im Code zu identifizieren und zu beheben. Indem man Nachrichten im standard error stream ausgibt, können Entwickler wichtige Informationen über den Zustand ihres Codes erhalten, was wiederum zu einer effizienteren Fehlerbehebung führt.

## Wie man schreibt zu standard error

Um Nachrichten in den Standardfehlerstrom zu schreiben, kann man die Funktion `write(_ string: String)` verwenden. Diese Funktion akzeptiert eine Zeichenfolge als Eingabe und gibt sie als Fehlermeldung aus. Hier ist ein Beispiel:

```Swift
write("Fehler beim Öffnen der Datei")
```

Dies würde die Nachricht "Fehler beim Öffnen der Datei" in den Standardfehlerstrom schreiben. Wenn man mehrere Zeichenfolgen oder Variablen ausgeben möchte, kann man den `+` Operator verwenden, um sie zu kombinieren. Hier ist ein Beispiel, das auch Variablen verwendet:

```Swift
let dateiName = "Benutzerdaten.txt"
write("Fehler beim Öffnen der Datei " + dateiName + ". Bitte überprüfe deine Datei.")
```

Die Ausgabe würde dann folgendermaßen aussehen:

`Fehler beim Öffnen der Datei Benutzerdaten.txt. Bitte überprüfe deine Datei.`

## Tief eintauchen

Eine tiefere Auseinandersetzung mit dem Schreiben von Fehlern zu standard error umfasst den Umgang mit Fehlern und die Verwendung von Optionals. Die Funktion `write(_ string: String)` gibt einen `NSError?` zurück, der verwendet werden kann, um auf Fehler zu reagieren. Man kann auch die Funktion `writeLine(_ string: String)` verwenden, um automatisch einen Zeilenumbruch anzuhängen. Es gibt auch mehrere Möglichkeiten, den Ausgabeort von Fehlermeldungen zu ändern, je nach den Anforderungen des Programms.

## Siehe auch

- [Swift-Dokumentation zur Fehlerbehandlung](https://developer.apple.com/documentation/swift/error_handling)
- [Wichtige Befehle in der Swift-Programmierung](https://www.raywenderlich.com/34-swift-glossary-of-important-commands)
- [Standard Error in Swift](https://www.avanderlee.com/swift/standard-error-output/)