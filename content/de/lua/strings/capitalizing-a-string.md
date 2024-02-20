---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:53.269950-07:00
description: "Das Kapitalisieren eines Strings beinhaltet die Modifikation des ersten\
  \ Buchstabens jedes Wortes in einem Satz zu einem Gro\xDFbuchstaben, w\xE4hrend\u2026"
lastmod: 2024-02-19 22:05:12.928703
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings beinhaltet die Modifikation des ersten\
  \ Buchstabens jedes Wortes in einem Satz zu einem Gro\xDFbuchstaben, w\xE4hrend\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings beinhaltet die Modifikation des ersten Buchstabens jedes Wortes in einem Satz zu einem Großbuchstaben, während sichergestellt wird, dass die restlichen Buchstaben klein geschrieben werden. Diese Technik wird üblicherweise verwendet, um Texte professioneller oder lesbarer zu gestalten, beispielsweise bei der Vorbereitung von Titeln oder Benutzereingaben zur Anzeige.

## Wie geht das:
Lua verfügt nicht über eine integrierte Funktion zum Kapitalisieren von Strings, aber Sie können diese Aufgabe mithilfe von grundlegenden String-Manipulationsfunktionen leicht bewältigen. Hier ist eine einfache Funktion, um den ersten Buchstaben eines einzelnen Wortes zu kapitalisieren:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hallo"))  -- Ausgabe: Hallo
```

Um jedes Wort in einem Satz zu kapitalisieren, können Sie den Satz in Worte aufteilen, jedes einzelne kapitalisieren und sie dann wieder zusammenführen:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hallo welt von lua"))  -- Ausgabe: Hallo Welt Von Lua
```

Wenn Sie an einem Projekt arbeiten, bei dem die Leistung entscheidend ist und Sie mehr fortgeschrittene String-Manipulationsfähigkeiten benötigen, erwägen Sie die Verwendung einer Drittanbieterbibliothek wie `Penlight`. Penlight erweitert Lua um vielseitigere Stringbearbeitungsfunktionen, unter anderem:

```lua
-- Unter der Annahme, dass Penlight installiert ist:
local pl = require("pl.stringx")
local text = "hallo lua benutzer"
text = pl.capitalized(text)
print(text)  -- Ausgabe: Hallo lua benutzer

-- Hinweis: Die capitalized-Funktion von Penlight kapitalisiert nur das erste Wort.
-- Um jedes Wort zu kapitalisieren, müssten Sie immer noch eine benutzerdefinierte Lösung implementieren oder andere Bibliotheken erkunden.
```
