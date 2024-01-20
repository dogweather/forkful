---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Strenginterpolering er prosessen med å bytte ut plassholdere i en streng med deres korresponderende verdier. Programvareutviklere bruker den for å bygge dynamiske strenger effektivt.

## Slik gjør du:

Her er et eksempel i Lua:

```Lua
navn = "Ola"
hilsen = "Hei, ".. navn .."!"
print(hilsen)
```

Dette vil skrive ut:

```
Hei, Ola!
```

## Dyp Dykk

Strenginterpolering ble først innført i programmeringsspråk på 1960-tallet, og det er et vanlig trekk i mange moderne språk. Lua tilbyr ikke innebygd støtte til strenginterpolering som noen andre språk (som JavaScript eller Ruby) gjør. Men, den bruker ".."-operatoren for å oppnå lignende resultat. Dessuten kan du velge å bruke en funksjon som `string.format` for mer komplekse scenarier.

## Se Også

For mer informasjon om strenginterpolering i Lua, sjekk ut disse kildene:

- [Lua Users Wiki: String Interpolation](http://lua-users.org/wiki/StringInterpolation)
- [Programming in Lua: Strings](https://www.lua.org/pil/2.4.html)