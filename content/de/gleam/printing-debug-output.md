---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:52:21.728958-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Drucke Debug-Ausgaben sind Textnachrichten, die während der Laufzeit eines Programms zur Fehlerbehebung ausgegeben werden. Programmierer nutzen sie, um zu verstehen, was im Code vor sich geht, und um Bugs leichter zu finden.

## Wie geht das?
In Gleam sind Druckausgaben direkt und unkompliziert. Hier ist ein simples Beispiel, wie du eine Debug-Nachricht ausgibst:

```gleam
import gleam/io

pub fn main() {
  io.debug("Hier ist eine Debug-Nachricht");
  // Weitere Logik deines Programms ...
}
```

Laufst du dieses Programm, siehst du die folgende Ausgabe:
```
Hier ist eine Debug-Nachricht
```

## Tiefere Einblicke
Historisch gesehen entstammen Drucke Debug-Ausgaben den Zeiten früher Konsolen und Teletypes, wo tatsächlich physische Ausdrucke für Debugging benutzt wurden. In modernen Sprachen wie Gleam bieten diverse Funktionen mehr Kontrolle und Formatierungsoptionen. Im Gegensatz zu `io.debug`, das einfache Textnachrichten ausgibt, gibt es komplexere Logging-Libraries, die zusätzliche Funktionen wie das Setzen von Log-Leveln und -Formaten oder das Ausgeben in Dateien bereitstellen. Implementierungsdetails variieren zwischen Programmiersprachen, aber das Grundprinzip bleibt: Informationen während der Laufzeit bereitstellen, um die Fehlersuche zu erleichtern.

## Siehe auch
- Gleam's offizielle Dokumentation über I/O: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Überblick über Logging in Programmiersprachen: https://en.wikipedia.org/wiki/Logging_(software)
- Diskussion über Debugging-Praktiken auf Stack Overflow: https://stackoverflow.com/questions/tagged/debugging