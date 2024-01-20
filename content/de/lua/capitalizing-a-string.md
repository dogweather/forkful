---
title:                "Einen String großschreiben"
html_title:           "Lua: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# In Großbuchstaben Umwandeln mit Lua

## Was & Warum?

Das Umwandeln eines Strings in Großbuchstaben bedeutet, jeden Buchstaben des Strings in seine Großbuchstabenversion zu ändern. Programmierer machen das, um Konsistenz bei Textvergleichen zu gewährleisten oder um Text hervorzuheben.

## Wie geht das:

Das Hervorheben eines Strings in Lua ist einfach. Verwende die `upper`-Methode. Hier ist ein Beispiel:

```lua
s = "hallo welt"
print(s:upper())
```

Ausführung dieses Codes gibt:

```
HALLO WELT
```

## Tiefere Erkenntnisse

Historisch gesehen wird diese Funktion oft verwendet, um Texteingaben von Benutzern zu normalisieren, indem alle Zeichen in Großbuchstaben umgewandelt werden. Dies gewährleistet bei Vergleichen Konsistenz, unabhängig von der Art und Weise, wie der Benutzer den Text eingegeben hat.

Einige Datenbank-Systeme und Sprachen, wie zum Beispiel SQL, verwenden diese Methode standardmäßig, um Textvergleiche durchzuführen. In anderen Sprachen wie Python gibt es ähnliche Funktionen wie `upper()`.

Lua implementiert die `upper`-Funktion in der Standardbibliothek und Sie können sie direkt an Strings anhängen. Beachte jedoch, dass alle nicht-alphabetischen Zeichen unverändert bleiben.

## Siehe auch:

- Lua 5.1 Referenzhandbuch: [Strings](https://www.lua.org/manual/5.1/manual.html#5.4)
- Lua-Handbuch: [string.upper](https://www.lua.org/manual/5.3/manual.html#pdf-string.upper)
- Wikipedia: [Unicode-Casing](https://en.wikipedia.org/wiki/Unicode_equivalence#Case_insensitivity)