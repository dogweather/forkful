---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:16:07.313745-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
REPL står for Read-Eval-Print Loop, et interaktivt miljø der du raskt kan teste kode. Programmerere bruker det til å eksperimentere, feilsøke og lære seg språkets særegenheter.

## Hvordan:
For å hoppe inn i Luas REPL, skriv bare `lua` i terminalen din. Her er et eksempelsesjon:

```Lua
> x = 10
> print(x * 2)
20
> t = {'eple', 'banan', 'kirsebær'}
> table.insert(t, 'dato')
> for i, frukt in ipairs(t) do print(i, frukt) end
1	eple
2	banan
3	kirsebær
4	dato
>
```
I sesjonen deklarerer vi en variabel, utfører grunnleggende aritmetikk, manipulerer en tabell og looper gjennom elementene.

## Dypdykk
Lua's lette natur gjør REPL-en ideell for prototyping. Den har vært rundt siden Lua's begynnelse på tidlig 1990-tallet, inspirert av tidligere interaktive skall for språk som Lisp. Alternativer i andre språk inkluderer `irb` for Ruby og `python` for Python, hver med sitt eget sett med funksjoner. Lua's REPL er minimalistisk; dermed kan det mangle avanserte funksjoner som finnes i andre, som komplekse feilsøkingsverktøy. For en mer fyldig opplevelse tilbyr verktøy som ZeroBrane Studio eller LuaDist's LuaRocks mer enn den grunnleggende REPL.

## Se også
- [Lua 5.4 Referansehåndbok - Den frittstående Lua-tolken](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
