---
title:                "Gleam: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf den Standardfehlerstrom (stderr) ist eine wichtige Funktion für Entwickler, um Fehlermeldungen und Debugging-Informationen in ihren Programmen zu erfassen. Durch die Verwendung von Gleam können wir dieses Feature auf einfache und effektive Weise in unsere Programme integrieren.

## Wie Geht's

Um auf den Standardfehlerstrom zu schreiben, können wir die Standardbibliotheksfunktion `io.format_stderr` verwenden. Diese Funktion akzeptiert einen Formatierungsstring und optionale Argumente, ähnlich wie die bekannte `printf`-Funktion in anderen Programmiersprachen.

```
Gleam
def some_function() {
  io.format_stderr("Oh nein, ein Fehler ist aufgetreten!")
}
```

Die Ausgabe dieses Codes wäre "Oh nein, ein Fehler ist aufgetreten!" auf dem stderr-Strom.

## Tiefgehender Einblick

Neben dem einfachen Schreiben von Text auf den Fehlerstrom können wir mit Gleam auch Fehlerobjekte auf den stderr-Strom schreiben. Diese können zusätzliche Informationen wie Fehlercodes und Stacktraces enthalten, die bei der Fehlersuche und Behebung nützlich sein können.

```
Gleam
type MyError {
  message: String,
  code: Int,
}

pub fn my_function() -> Result<(), MyError> {
  // ...code
}

let error = MyError {message: "Da ist was schief gelaufen", code: 500}
io.error(error)
```

Die Ausgabe in diesem Fall wäre:

```
MyError { message: "Da ist was schief gelaufen", code: 500 }
```

## Siehe auch

- [Gleam-Dokumentation zur io-Bibliothek](https://gleam.run/articles/standard-library-io/)
- [Gleam-Dokumentation zu Fehlerbehandlung](https://gleam.run/articles/error-handling/)