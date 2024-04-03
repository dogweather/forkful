---
date: 2024-01-20 17:46:10.667757-07:00
description: 'How to: In Lua, usiamo `string.sub` per estrarre sottostringhe. Ecco
  un esempio semplice.'
lastmod: '2024-03-13T22:44:43.547591-06:00'
model: gpt-4-1106-preview
summary: In Lua, usiamo `string.sub` per estrarre sottostringhe.
title: Estrazione di sottostringhe
weight: 6
---

## How to:
In Lua, usiamo `string.sub` per estrarre sottostringhe. Ecco un esempio semplice:

```Lua
local testo = "Ciao, mondo di Lua!"
local sottostringa = string.sub(testo, 7, 11)
print(sottostringa) -- Stampa "mondo"
```

Output:
```
mondo
```

Per prendere i caratteri fino alla fine della stringa:

```Lua
local fine = string.sub(testo, 13)
print(fine) -- Stampa "di Lua!"
```

Output:
```
di Lua!
```

## Deep Dive
Estrarre sottostringhe è una pratica standard nel mondo della programmazione, storicamente usata nei linguaggi più vecchi come C con funzioni come `strncpy`. In Lua, `string.sub` è la funzione integrata per questa operazione, ma ci sono alternative come la libreria `stringx` che offre ulteriori funzionalità. A livello di implementazione, Lua gestisce le sottostringhe in modo efficiente, ma è sempre buona norma fare attenzione alle prestazioni quando si lavora con stringhe molto lunghe o operazioni ripetute molte volte.

## See Also
Per approfondire, guarda la documentazione ufficiale:
- Funzioni delle stringhe in Lua: https://www.lua.org/manual/5.4/manual.html#6.4
- Libreria esterna `stringx`: http://github.com/davidm/lua-stringx

E altre risorse utili:
- Tutorial Lua sui pattern: https://www.lua.org/pil/20.2.html
- Community Lua: https://www.lua.org/community.html
