---
title:                "Schreiben auf den Standardfehler"
html_title:           "Swift: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Programmieren gibt es oft die Notwendigkeit, Fehlermeldungen oder wichtige Informationen auszugeben, jedoch ohne den normalen Ausgabe-Stream zu stören. Das ist genau der Zweck des Schreibens auf den Standardfehler. Es ist eine Möglichkeit, Nachrichten direkt an die Konsole zu senden, um dem Entwickler wichtige Informationen mitzuteilen, ohne die eigentliche Ausgabe des Programms zu beeinflussen.

# Wie geht's?
Um etwas auf den Standarderror zu schreiben, verwendet man die Funktion `write(_:)` und übergibt ihr eine String-Nachricht. Hier ist ein Beispielcode:

```Swift
write("Dies ist eine wichtige Fehlermeldung.", to: &standardError)
```

Das Ergebnis wird dann in der Konsole, unabhängig von der normalen Ausgabe des Programms, angezeigt:

```Swift
Dies ist eine wichtige Fehlermeldung.
```

Man kann auch den Standardfehlerstream direkt mit dem `standardError` Konstante referenzieren und somit die `write(_:)` Funktion weglassen:

```Swift
standardError.write("Noch eine wichtige Nachricht.")
```

# Tiefergehender Einblick
Das Schreiben auf den Standarderror wird hauptsächlich für die Fehlerbehandlung und Debugging-Zwecke verwendet. Anstatt die Nachricht auf dem normalen Ausgabestream zu drucken, bleibt sie auf dem Standardfehler und kann somit besser von anderen Programmausgaben unterschieden werden.

Eine Alternative zum Schreiben auf den Standarderror ist das Verwenden von benutzerdefinierten Log-Dateien oder dem Debugging-Tool des jeweiligen IDEs. Jedoch ist das Schreiben auf den Standarderror eine schnelle und unkomplizierte Möglichkeit, wichtige Informationen direkt an die Konsole zu senden.

Zur Umsetzung des Schreibens auf den Standarderror verwendet Swift eine Konstante namens `standardError`, welche einen Pointer auf den Standardfehlerstream darstellt. Durch die Verwendung von Referenzen, ist es möglich, direkt auf diesen Stream zu schreiben, ohne die normale Ausgabe des Programms zu beeinflussen.

# Sieh dir auch an
- [Stack Overflow Diskussion über das Schreiben auf den Standarderror in Swift](https://stackoverflow.com/questions/24008958/writing-to-standard-error-in-swift)