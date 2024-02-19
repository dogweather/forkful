---
aliases:
- /fi/lua/using-an-interactive-shell-repl/
date: 2024-01-26 04:16:13.026372-07:00
description: "REPL tarkoittaa Read-Eval-Print Loopia, interaktiivista ymp\xE4rist\xF6\
  \xE4, jossa voit nopeasti testata koodia. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4\
  \ kokeiluun,\u2026"
lastmod: 2024-02-18 23:09:07.758743
model: gpt-4-0125-preview
summary: "REPL tarkoittaa Read-Eval-Print Loopia, interaktiivista ymp\xE4rist\xF6\xE4\
  , jossa voit nopeasti testata koodia. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 kokeiluun,\u2026"
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?
REPL tarkoittaa Read-Eval-Print Loopia, interaktiivista ympäristöä, jossa voit nopeasti testata koodia. Ohjelmoijat käyttävät sitä kokeiluun, vianmääritykseen ja kielen erikoisuuksien opetteluun.

## Miten:
Hypätäksesi Luas REPL:iin, kirjoita vain `lua` terminaaliisi. Tässä on esimerkki istunnosta:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
Istunnossa julistamme muuttujan, teemme perusaritmetiikkaa, manipuloimme taulukkoa ja käymme sen kohteita läpi.

## Syvä sukellus
Luas keveys tekee sen REPL:stä ideaalin prototyyppien tekemiseen. Se on ollut olemassa Luas perustamisesta alkaen, varhaisissa 1990-luvulla, inspiroitunut aikaisemmista interaktiivisista kuorista kielille kuten Lisp. Vaihtoehtoja muissa kielissä ovat esimerkiksi `irb` Rubylle ja `python` Pythonille, joissa kullakin on omat ominaisuutensa. Luas REPL on minimalistinen; joten se saattaa jäädä vaille muiden löytyviä edistyneitä ominaisuuksia, kuten monimutkaisia vianmääritystyökaluja. Runsampaan kokemukseen, työkalut kuten ZeroBrane Studio tai LuaDistin LuaRocks tarjoavat enemmän kuin perus REPL.

## Katso myös
- [Lua 5.4 Referenssikäsikirja - Erillinen Lua-tulkki](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
