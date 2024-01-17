---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "Python: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was ist es und warum?

In der Programmierung gibt es häufig Situationen, in denen wir Fehler- oder Debuginformationen ausgeben möchten, aber diese Ausgaben nicht den normalen Output stören sollen. Hier kommt das Schreiben auf den Standardfehler (englisch: standard error) ins Spiel. Es ist eine Möglichkeit, solche Informationen getrennt vom normalen Output auszugeben.

Programmierer nutzen das Schreiben auf den Standardfehler, um diese zwei Arten von Ausgaben zu unterscheiden und gezielter zu verarbeiten. So können sie beispielsweise Fehlermeldungen an den Benutzer ausgeben, während sie gleichzeitig Debuginformationen für ihre eigenen Zwecke auf den Standardfehler schreiben.

## Wie man es macht:

Das Schreiben auf den Standardfehler ist in Python sehr einfach. Wir verwenden dafür das `sys` Modul und die Funktion `stderr`:

    ```Python
    import sys

    sys.stderr.write("Diese Information wird auf den Standardfehler geschrieben.")
    ```

Die Ausgabe erscheint dann in roter Schrift, um sie von normalen Output zu unterscheiden. Hier ist ein Beispiel der Ausgabe:

    ```
Diese Information wird auf den Standardfehler geschrieben.
    ```

## Tiefer eintauchen:

Das Schreiben auf den Standardfehler ist ein Teil des Streams-Konzepts in der Programmierung. Dabei gibt es drei Arten von Streams: `stdin`, `stdout` und `stderr`. `stdin` steht für die Standard-Eingabe, `stdout` für die Standard-Ausgabe und `stderr` für den Standardfehler.

Historisch gesehen wurde das Schreiben auf den Standardfehler in der UNIX-Philosophie eingeführt, um zwischen normaler und fehlerhafter Ausgabe zu unterscheiden. In Python gibt es auch die Möglichkeit, Fehlermeldungen auf den Standardfehler zu schreiben, indem man das `raise` Statement verwendet.

Als Alternative zum Schreiben auf den Standardfehler gibt es auch das Logging-Modul in Python, welches eine umfangreichere und strukturiertere Möglichkeit bietet, Informationen auszugeben.

## Weitere Informationen:

Wenn du mehr über das Schreiben auf den Standardfehler erfahren möchtest, kannst du diese Quellen konsultieren:

- https://www.python.org/dev/peps/pep-0498/
- https://docs.python.org/3/library/sys.html
- https://docs.python.org/3/howto/logging.html