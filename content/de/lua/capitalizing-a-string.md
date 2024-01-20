---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, jeden Buchstaben des Strings in einen Großbuchstaben umzuwandeln. Programmierer verwenden es, um Konsistenz in Nutzereingaben zu gewährleisten oder um Texte gemäß bestimmten Formatanforderungen zu standardisieren.

## So geht's:
```Lua
-- Konvertierung eines Strings zu Großbuchstaben in Lua
local text = "hallo, wie geht es dir?"
local capitalizedText = text:upper()

print(capitalizedText)  -- Ausgabe: "HALLO, WIE GEHT ES DIR?"
```

## Tiefgang
In den frühen Tagen der Computerprogrammierung war die Unterscheidung zwischen Groß- und Kleinschreibung oft bedeutungslos; Computer und Programme nutzten Bezeichner in konstanten Formaten. Heute wird das Großschreiben genutzt, um benutzergenerierte Inhalte zu normieren oder einen String für bestimmte Anzeigekontexte zu formatieren.

Lua bietet mit der `upper`-Methode der String-Bibliothek eine eingebaute Funktion, um Strings komplett in Großbuchstaben zu konvertieren. Alternativen wie manuelle Iteration über Charaktere und ASCII-Werte-Umwandlung sind unnötig kompliziert und nicht empfehlenswert.

Die `upper`-Methode ist effizient und direkt; sie kümmert sich um die Besonderheiten der Zeichenkodierung, sodass Entwickler sich nicht mit einzelnen Codepunkten und deren Transformationen auseinandersetzen müssen.

## Siehe auch
1. Lua-Handbuch zur `upper`-Methode: [http://www.lua.org/manual/5.4/manual.html#pdf-string.upper](http://www.lua.org/manual/5.4/manual.html#pdf-string.upper)
2. Artikel über Unicode und Zeichenkodierung in Lua: [https://www.lua.org/pil/21.2.2.html](https://www.lua.org/pil/21.2.2.html)