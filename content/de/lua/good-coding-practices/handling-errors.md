---
date: 2024-01-26 00:54:59.577326-07:00
description: "Wie geht das: Lua verwendet zwei Hauptfunktionen f\xFCr die Fehlerbehandlung:\
  \ `pcall` und `xpcall`. So setzt man sie ein."
lastmod: '2024-03-13T22:44:54.022567-06:00'
model: gpt-4-1106-preview
summary: "Lua verwendet zwei Hauptfunktionen f\xFCr die Fehlerbehandlung."
title: Fehlerbehandlung
weight: 16
---

## Wie geht das:
Lua verwendet zwei Hauptfunktionen für die Fehlerbehandlung: `pcall` und `xpcall`. So setzt man sie ein:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Hoppla! Etwas ist schiefgelaufen.")
    else
        print("Alles gut!")
    end
end

-- Verwendung von pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Erfolg!")
else
    print("Einen Fehler gefangen:", errorMessage)
end

-- Verwendung von xpcall mit einem Fehlerbehandlungsroutine
function myErrorHandler(err)
    print("Fehlerbehandlungsroutine sagt:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("War der Aufruf erfolgreich?", status)
```

Eine mögliche Ausgabe könnte sein:

```
Einen Fehler gefangen: Hoppla! Etwas ist schiefgelaufen.
Fehlerbehandlungsroutine sagt: Hoppla! Etwas ist schiefgelaufen.
War der Aufruf erfolgreich? false
```
Oder, wenn kein Fehler auftritt:
```
Alles gut!
Erfolg!
Alles gut!
War der Aufruf erfolgreich? true
```

## Vertiefung
Fehlerbehandlung oder "Ausnahmebehandlung" war nicht immer üblich. Frühe Programme stürzten häufig ab. Mit der Entwicklung der Programmierung wuchs auch das Bedürfnis nach Stabilität. Luas Ansatz ist im Vergleich zu einigen Sprachen einfach. Es gibt keine `try/catch` Blöcke, nur `pcall` und `xpcall`. Erstere schützt einen Funktionsaufruf und liefert einen Status und etwaige Fehler zurück. Letztere fügt eine Fehlerbehandlungsfunktion hinzu, die für individuelles Aufräumen oder Protokollieren nützlich ist.

Eine Alternative in Lua ist die Verwendung von `assert`, die einem ähnlichen Zweck dient, indem sie einen Fehler auslöst, wenn ihre Bedingung falsch ist. Aber sie ist nicht so flexibel wie `pcall` für komplexe Fehlerbehandlungsszenarien.

Intern arbeiten `pcall` und `xpcall`, indem sie eine "geschützte Umgebung" für das Ausführen der Funktion einrichten. Wenn ein Fehler auftaucht, fängt die Umgebung ihn ab und kann ihn entweder direkt behandeln oder an das Programm zurückgeben, um ihn zu behandeln.

## Siehe auch
- Das Buch "Programming in Lua" (dritte Ausgabe), erhältlich unter https://www.lua.org/pil/, für eine gründliche Lektüre über Fehlerbehandlung (Abschnitt 8.4).
- Offizielles Lua 5.4 Referenzhandbuch: https://www.lua.org/manual/5.4/ - für die aktuellsten Informationen zu Luas Fehlerbehandlungsfunktionen.
- Lua-users Wiki über Fehlerbehandlung: http://lua-users.org/wiki/ErrorHandling - für Gemeinschaftseinblicke und -muster.
