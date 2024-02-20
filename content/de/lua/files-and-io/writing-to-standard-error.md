---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:45.872471-07:00
description: "Das Schreiben auf Standardfehler (stderr) bedeutet, Fehlermeldungen\
  \ und diagnostische Ausgaben auf einen separaten Kanal zu leiten, getrennt von der\u2026"
lastmod: 2024-02-19 22:05:12.962379
model: gpt-4-0125-preview
summary: "Das Schreiben auf Standardfehler (stderr) bedeutet, Fehlermeldungen und\
  \ diagnostische Ausgaben auf einen separaten Kanal zu leiten, getrennt von der\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standardfehler (stderr) bedeutet, Fehlermeldungen und diagnostische Ausgaben auf einen separaten Kanal zu leiten, getrennt von der Standardausgabe (stdout). Programmierer tun dies, um reguläre Programmergebnisse von Fehlerinformationen zu unterscheiden, was das Debuggen und Protokollieren vereinfacht.

## Wie:
In Lua kann das Schreiben auf stderr mit der Funktion `io.stderr:write()` erreicht werden. Hier ist, wie Sie eine einfache Fehlermeldung an Standardfehler schreiben können:

```lua
io.stderr:write("Fehler: Ungültige Eingabe.\n")
```

Sollten Sie eine Variable ausgeben müssen oder mehrere Datenstücke kombinieren wollen, fügen Sie diese innerhalb der Schreibfunktion zusammen:

```lua
local errorMessage = "Ungültige Eingabe."
io.stderr:write("Fehler: " .. errorMessage .. "\n")
```

**Beispielausgabe auf stderr:**
```
Fehler: Ungültige Eingabe.
```

Für komplexere Szenarien oder bei der Arbeit mit größeren Anwendungen könnten Sie Bibliotheken von Drittanbietern für die Protokollierung in Betracht ziehen, wie LuaLogging. Mit LuaLogging können Sie Protokolle an verschiedene Ziele lenken, einschließlich stderr. Hier ist ein kurzes Beispiel:

Zuerst stellen Sie sicher, dass LuaLogging mit LuaRocks installiert ist:

```
luarocks install lualogging
```

Dann, um eine Fehlermeldung mit LuaLogging an stderr zu schreiben:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Fehler: Ungültige Eingabe.")
```

Dieser Ansatz bietet den Vorteil einer standardisierten Protokollierung in Ihrer Anwendung, mit der zusätzlichen Flexibilität, Protokollebenen (z. B. ERROR, WARN, INFO) durch eine einfache API festzulegen.
