---
title:                "Schreiben in den Standardfehler"
html_title:           "Lua: Schreiben in den Standardfehler"
simple_title:         "Schreiben in den Standardfehler"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standardfehler ist ein Weg für Programmierer, Fehler und andere wichtige Informationen anzuzeigen, die während der Ausführung ihres Codes auftreten können. Es ist nützlich für die Fehlersuche und Erstellung von Protokollen, um zu verstehen, was Ihr Code tut und wo mögliche Probleme auftreten können.

## Wie geht es?
Das Schreiben auf Standardfehler in Lua ist einfach. Sie müssen nur die Standardfehlerfunktion (engl. Standard Error Function) verwenden, die als "io.stderr" in Lua bekannt ist. Wir können ihr eine Zeichenkette übergeben, die wir ausgeben möchten, um sie auf dem Bildschirm auszudrucken.

```Lua
io.stderr:write("Dies ist eine wichtige Fehlermeldung.")
```

Die Ausgabe dieser Zeile würde im Terminal folgendermaßen aussehen:

```
Dies ist eine wichtige Fehlermeldung.
```

## Tief einsteigen
Das Schreiben auf Standardfehler ist eine weit verbreitete Praxis in der Programmierung, um die Fehlerbehebung und das Verständnis des Codes zu verbessern. Auf diese Weise können Programmierer wichtige Informationen anzeigen, ohne den normalen Ablauf des Programms zu stören. Es ist auch eine Alternative zum Schreiben auf Standardausgabe (engl. Standard Output), die möglicherweise für die Anzeige von Benutzerinformationen verwendet wird.

In Lua gibt es auch die Funktion "io.stdout", die standardmäßig für die Standardausgabe verwendet wird. Programmierer können jedoch auch die Funktion "print()" verwenden, um an die Standardausgabe zu schreiben. Im Vergleich dazu ist das Schreiben auf Standardfehler mit "io.stderr:write()" expliziter und bietet mehr Kontrolle über die Ausgabe.

## Siehe auch
- Offizielle Lua-Dokumentation zu io.stderr: https://www.lua.org/manual/5.4/manual.html#6.8.2
- Vergleich von Standardfehler und Standardausgabe in Lua: https://stackoverflow.com/questions/3448147/lua-print-vs-stdout-vs-stderr