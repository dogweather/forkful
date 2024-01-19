---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Il parsing delle date da una stringa in Lua consiste nell'estrarre e interpretare una specifica data da una stringa di testo. Questa pratica è diffusa in programmazione, ad esempio, quando i dati vengono ricevuti sotto forma di JSON o XML e devono essere convertiti in un oggetto data manipolabile.

## Come fare:

Diamo un'occhiata all'azione utilizzando il modulo os.date in Lua:

```Lua
data = "03/21/1997"
 
mese, giorno, anno = data:match("(%d+)/(%d+)/(%d+)")
 
dataT = os.time({year=anno, month=mese, day=giorno})
  
print(os.date('%A, %B %d, %Y', dataT))
```

Una possibile uscita da questo codice potrebbe essere:

```Lua
Venerdì, marzo 21, 1997
```

Questo codice esegue il parsing della data in formato americano MM/DD/YYYY, la converte in un oggetto Lua data e infine la stampa in un formato leggibile.

## Approfondimenti

Il modulo os.date di Lua deriva dalla libreria C che era disponibile dalla prima versione di Lua. Offre funzionalità per manipolare la data e l'ora. Alternativamente, ci sono altre librerie come "date" e "Penlight" che offrono funzionalità più avanzate.

Se vuoi un parsing più complesso, è possibile utilizzare le espressioni regolari Lua. Questi sono spesso molto potenti, ma possono diventare rapidamente complessi.

## Vedi anche

Guarda queste risorse per saperne di più su date e stringhe in Lua:

1. [Lua Users Wiki: Dates and Time](http://lua-users.org/wiki/DatesAndTime)
2. [Lua Documentation: os.date](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
3. [Penlight Documentation](https://stevedonovan.github.io/Penlight/api/index.html)