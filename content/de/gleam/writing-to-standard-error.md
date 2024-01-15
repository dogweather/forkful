---
title:                "Schreiben auf den Standardfehler"
html_title:           "Gleam: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte jemand standard error (die Standardfehlerausgabe) beim Programmieren nutzen? Nun, standard error ist ein Weg, um wichtige Fehlermeldungen und Informationen während der Ausführung des Programms anzuzeigen. Es kann auch dabei helfen, spezifische Probleme zu identifizieren und zu debuggen.

## Wie funktioniert es
Um standard error zu nutzen, musst du zunächst das Modul 'std.err' importieren. Dann kannst du die Funktion 'stdio.err' aufrufen und als Argument den zu zeigenden Text übergeben. Hier ist ein Beispiel:

```Gleam
import std.err

let greetings = "Hallo Welt!"
stdio.err(greetings)
```

Das Ergebnis wird in der Konsole ausgegeben:
```sh
Hallo Welt!
```

## Tiefergehende Informationen

Während 'stdio.err' die Funktionalität liefert, um standard error zu nutzen, gibt es einige wichtige Aspekte, die du beachten solltest. Wenn du mehrere Werte ausgeben möchtest, solltest du ein Array von Strings an 'stdio.err' übergeben. Zudem ist es eine gute Praxis, die Ausgaben von standard error anders zu formatieren als die von standard output (die Standardausgabe). Dies kann helfen, Fehlermeldungen und andere wichtige Informationen besser zu unterscheiden.

## Siehe auch
- [Gleam-Dokumentation für std.err](https://gleam.run/modules/std-err/)
- [Tutorial zu Gleam-Programmierung](https://gleam.run/book/introduction.html)
- [Diskussion zu standard error und standard output](https://www.reddit.com/r/gleam/comments/fyrprf/stderr_vs_stdout/)