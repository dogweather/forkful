---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
simple_title:         "Schreiben auf Standardfehler"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben in den Standardfehler (stderr) ermöglicht es, Fehlermeldungen getrennt von regulären Ausgabedaten (stdout) zu behandeln. Dies ist nützlich, um die Logik von Programmfluss und Fehlern zu trennen, was die Analyse und das Debuggen erleichtert.

## So geht's:
```Swift
import Foundation

// Schreibe in Standardausgabe
print("Hello, stdout!")

// Schreibe in Standardfehler
let stderr = FileHandle.standardError
if let data = "Error: Something went wrong!\n".data(using: .utf8) {
    stderr.write(data)
}

// Beispiel für kombinierte Ausgabe in der Konsole:
// Hello, stdout!
// Error: Something went wrong!
```

## Tiefgang:
Historisch kommt die Trennung von `stdout` und `stderr` aus der Unix-Welt, wo sie die flexible Weiterleitung von Logs und Benutzerdaten ermöglichte. Alternativ kann man auch `NSLog()` nutzen, wobei dies eher für Logging als für Fehlermeldungen gedacht ist. Unter der Haube leitet Swift `stderr` an den File Descriptor 2, der konventionsgemäß für Fehlermeldungen reserviert ist.

## Siehe auch:
- Apple's Swift-Dokumentation: [Swift.org](https://www.swift.org/documentation/)
- Stack Overflow Diskussionen zum Umgang mit `stderr` in Swift
- Unix Standard-Streams in "Advanced Programming in the UNIX Environment" von W. Richard Stevens
