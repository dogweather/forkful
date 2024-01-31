---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Standardfehler (stderr) ist ein Ausgabekanal, der für Fehlermeldungen und Diagnostik genutzt wird, um sie von regulären Ausgaben (stdout) zu trennen. Programmierer verwenden stderr, um Probleme und wichtige Warnungen sichtbar zu machen, ohne den normalen Outputflow zu stören.

## So geht's:
In Lua schreiben wir mit `io.stderr:write()` auf stderr.

```lua
-- Schreibt eine Warnmeldung auf stderr
io.stderr:write("Warnung: Etwas lief schief!\n")

-- Normale Ausgabe auf stdout
print("Das ist eine normale Ausgabe.")
```

Ausgabe im Terminal könnte so aussehen:
```
Warnung: Etwas lief schief!
Das ist eine normale Ausgabe.
```

## Deep Dive
Stderr hat seine Wurzeln in UNIX-Systemen und ist ein Standarddateistream, der für Fehlermeldungen vorgesehen ist. Alternativen wie Logging in Dateien oder die Verwendung dedizierter Logging-Frameworks sind gebräuchlich, können aber die direkte und schnelle Natur von stderr beeinträchtigen. In Lua geht die Implementierung von stderr über die standardisierte `io`-Bibliothek, wobei `io.stderr` eine Datei repräsentiert, die für Fehlerausgaben reserviert ist. Da stderr unabhängig von stdout ist, können Ausgaben umgeleitet oder separat behandelt werden.

## Siehe auch
- Lua 5.4 Referenzhandbuch: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- `io`-Bibliothek in Lua: [https://www.lua.org/pil/21.html](https://www.lua.org/pil/21.html)
- Mehr zu stdout und stderr: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
