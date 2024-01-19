---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å flette sammen strenge, eller 'string concatenation' som det heter på engelsk, er når vi slår sammen to eller flere tekststrenger til én. Dette er meget anvendelig for å formatere uttrykk, legge sammen tekstfragmenter, og generere dynamisk innhold.

## Hvordan Gjør Man Det:

Lua bruker operatoren '..' for å flette sammen to strenger. Her kommer noen eksempler på hvordan det gjøres:

```Lua
-- Eksempel 1
tekst1 = "Hallo, "
tekst2 = "verden!"
sammen = tekst1 .. tekst2
print(sammen)  -- skriver ut: Hallo, verden!

-- Eksempel 2
navn = "Ola"
hilsen = "Hei, " .. navn
print(hilsen)  -- skriver ut: Hei, Ola
```

Du kan også flette sammen tall med tekststrenger, Lua vil automatisk omforme tallet til tekst.

```Lua
-- Eksempel 3
alder = 25
tekst = "Du er " .. alder .. " år gammel."
print(tekst)  -- skriver ut: Du er 25 år gammel.
```

## Detaljer

Historisk sett, bærer fletting av tekststrenger i Lua preg av sitt utgangspunkt som et skriptspråk for konfigurasjon og lett automatisering. Det er konstruert for å være kompakt, og dette reflekteres i den enkle og konsise syntaksen for å flette strenger.

Esoteriske alternativer for å flette sammen strenge finnes, som bruk av funksjonen string.format eller string.gsub. Disse kan gi mer kontroll over formatteringen, men er mer detaljerte.

Å forstå detaljene i implementasjonen kan hjelpe programmereren til å bedre utnytte funksjonen. Når to strenge flettes sammen i Lua, lager det en ny streng som holder det kombinerte innholdet. Dette kan ha implikasjoner for minnebruk og ytelse dersom du jobber med veldig store strenger. 

## Se Også

1. Lua 5.4 Reference Manual on Strings: [https://www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
2. Programming in Lua, String Manipulation [https://www.lua.org/pil/20.html](https://www.lua.org/pil/20.html)
3. Lua-Users String Recipes: [http://lua-users.org/wiki/StringRecipes](http://lua-users.org/wiki/StringRecipes)

Husk at kunnskap er makt. God kode!