---
title:                "Anführungszeichen aus einem String entfernen"
aliases:
- /de/lua/removing-quotes-from-a-string/
date:                  2024-01-26T03:40:20.310137-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Anführungszeichen aus einem String zu entfernen bedeutet, diese doppelten oder einfachen Anführungszeichen, die deinen Text umarmen, wegzuschälen. Programmierer machen das, um Eingaben zu bereinigen, das Parsen zu erleichtern oder Daten zu harmonisieren, die möglicherweise inkonsistent zitiert sind.

## Wie:
So wirst du die Anführungszeichen in Lua los:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hallo, Welt!"'))     -- Hallo, Welt!
print(remove_quotes("'Auf Wiedersehen, Anführungszeichen!'"))  -- Auf Wiedersehen, Anführungszeichen!
```

Bingo! Diese Anführungszeichen sind verschwunden wie Socken in einem Trockner.

## Tiefergehend
Leute entfernen Anführungszeichen von Strings, seitdem Sprachen Text verarbeiten können, was praktisch schon immer der Fall ist. In Lua übernimmt die Funktion `gsub` die schwere Arbeit und nutzt Muster wie ein Skalpell, um Anführungszeichen herauszuschneiden. Alternativen? Sicher, du könntest Regex in Sprachen verwenden, die es unterstützen, oder deine eigene Schleife schreiben, die jeden Charakter durchgeht (gähn, aber hey, es ist deine Zeit).

Lua's Musterabgleich bietet dir die Wucht eines Regex-Lite-Erlebnisses, ohne eine ganze Bibliothek zu importieren. Das Caret-Zeichen (`^`) und das Dollarzeichen (`$`) passen jeweils auf den Anfang und das Ende des Strings; `%p` passt auf jedes Satzzeichen. Nachdem wir die führenden und endenden Satzzeichen abgeschüttelt haben, fangen wir alles andere mit `(.*),` und ersetzen den gesamten Treffer mit dieser Erfassungsgruppe unter Verwendung von `" %1"`.

Denke daran, dass Luas Musterabgleich nicht so mächtig ist wie vollwertige Regex-Engines - zum Beispiel kann er nicht zählen oder zurückverfolgen. Diese Einfachheit ist je nachdem, welche Anführungszeichen du gerade einfängst und wo sie sich verstecken, sowohl ein Segen als auch ein Fluch.

## Siehe auch
Tauche tiefer in Luas Musterabgleich ein mit dem Buch „Programming in Lua“ (PiL): http://www.lua.org/pil/20.2.html

Für pure Eleganz, sieh dir an, wie es andere Sprachen machen, angefangen mit Pythons `str.strip`: https://docs.python.org/3/library/stdtypes.html#str.strip
