---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?

Att generera slumpmässiga nummer i programmering används för att skapa oförutsägbara data. Programmerare gör det för att simulera slumpmässiga händelser, till exempel i spel och kryptering.

## Hur Man Gör:

Här är hur man genererar ett slumpmässigt nummer i Lua.

```Lua
math.randomseed(os.time())

random_number = math.random()

print(random_number)
```
Exempel på output kan vara `0.0012512588889884` eller något annat slumpmässigt nummer mellan 0 och 1.

## Djupdykning

Historiskt sett både mjukvara och hårdvara har använts för att generera slumpmässiga nummer. Till exempel, tidiga datorer använde brus från radiofrekvenser. 

Alternativen till `math.random` i Lua inkluderar externa bibliotek som har mer avancerade funktioner. JavaScript, till exempel, har `Math.random()` och Python har `random.random()`.

Implementationen av `math.random` i Lua baseras på C-funktionen `rand`, vilket innebär att dess kapacitet och begränsningar är samma som för `rand`.

## Se Även

För vidare läsning och mer ingående förståelse, här är några länkar:
1. Official Lua Documentation: [math.random](https://www.lua.org/manual/5.3/manual.html#6.7)
2. Tutorialspoint: [Lua - Math.random function](https://www.tutorialspoint.com/lua/lua_math_random.htm)

---