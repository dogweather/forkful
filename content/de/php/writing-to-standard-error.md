---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"Was & Warum?"

Standardfehler (stderr) ist ein Ausgabekanal, der für die Protokollierung von Fehlern genutzt wird, während Standardausgabe (stdout) für normale Programmoutput steht. Programmierer nutzen stderr, um Fehlermeldungen von anderen Ausgaben zu trennen, was das Debugging und die Logfile-Analyse erleichtert.

## How to:
"Wie geht's?"

PHP ermöglicht das Schreiben auf stderr mit dem `fwrite()`-Funktion oder dem `file_put_contents()`-Funktion, zusammen mit der Konstante `STDERR`. Hier ein Beispiel, wie man eine einfache Fehlermeldung schreibt:

```php
<?php
// Schreiben auf Standardfehler
fwrite(STDERR, "Ein Fehler ist aufgetreten.\n");

// Alternativer Weg mit file_put_contents()
file_put_contents('php://stderr', "Ein alternativer Fehler.\n");
```

Ausgabe könnte so aussehen (je nach Umgebung eventuell nur im Fehlerlogs sichtbar):

```
Ein Fehler ist aufgetreten.
Ein alternativer Fehler.
```

## Deep Dive
"Deep Dive"

Früher war es üblich, stderr über Shell-Umleitungen oder separate Error-Log-Dateien zu verwenden. In PHP gibt es `STDERR` seit Version 4.3.0, und es ist ein vordefinierter Dateihandle, der ohne Öffnung direkt verwendet werden kann. Die Alternativen zum stderr wie Logging-Bibliotheken (z.B. Monolog) bieten mehr Flexibilität und Funktionen, sind aber für komplexe Anwendungen überdimensioniert, wenn man einfach nur schnelle Fehlermeldungen ausgeben möchte.

## See Also
"Siehe auch"

Für weiterführende Informationen:

- PHP.net on Standard Predefined Constants: https://www.php.net/manual/en/reserved.constants.php
- Stack Overflow on When to use STDERR instead of STDOUT: https://stackoverflow.com/questions/1430956/when-to-use-stderr-instead-of-stdout
- PHP The Right Way on Error Reporting: http://www.phptherightway.com/#error_reporting