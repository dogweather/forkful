---
title:                "Interpolazione di una stringa"
html_title:           "Lua: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'interpolazione di stringhe è il processo di amalgamare variabili o espressioni all'interno di una stringa. Ciò è utile per costruire stringhe dinamiche che cambiano in base alle variabili o condizioni all'interno di un programma. I programmatori utilizzano l'interpolazione di stringhe per rendere il loro codice più efficiente e leggibile.

## Come fare:
Ecco un esempio di come usare l'interpolazione di stringhe in Lua:

```Lua
-- Dichiarazione di variabili
nome = "Mario"
anni = 25

-- Interpolazione di stringhe
saluto = "Ciao, mi chiamo ${nome} e ho ${anni} anni."
print(saluto) 
```

Output: 
Ciao, mi chiamo Mario e ho 25 anni.

Come puoi vedere, l'interpolazione di stringhe viene utilizzata all'interno delle doppie parentesi graffe ${...}. Ciò indica al programma che quelle variabili o espressioni devono essere sostituite all'interno della stringa.

## Approfondimento:
L'interpolazione di stringhe non è un concetto nuovo nel mondo della programmazione. È stato introdotto in linguaggi come Perl e Ruby e successivamente adottato in molti altri linguaggi. In Lua, è anche possibile utilizzare la funzione string.format() per formattare ed elaborare stringhe con variabili. Tuttavia, l'interpolazione di stringhe è spesso preferita per la sua semplicità e chiarezza.

## Vedi anche:
- [Documentazione ufficiale di Lua](https://www.lua.org/)
- [Articolo su interpolazione di stringhe in altri linguaggi di programmazione](https://en.wikipedia.org/wiki/String_interpolation)
- [Altro esempio di utilizzo dell'interpolazione di stringhe in Lua](https://www.tutorialspoint.com/lua/lua_string_interpolation.htm)