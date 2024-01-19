---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertire una Data in una Stringa in Lua: Tutto Quello che Devi Sapere

## Cosa & Perché?
Convertire una data in una stringa significa trasformare una rappresentazione di un istante temporale in una serie di caratteri leggibili. Programmatori lo fanno per migliorare la leggibilità dei dati o per poter manipolare la data come dato testuale.

## Come Fare
Usa la funzione `os.date`. Per esempio:

```Lua
data_corrente = os.date('%c')  
print(data_corrente) 
```

Questo codice stampa un timestamp. Il suo output potrebbe essere: `Tue Dec 7 21:53:51 2021`.

Per una data personalizzata, fai così:

```Lua
data_personalizzata = os.date('%A %d %B %Y', os.time{year=2030, month=12, day=25})
print(data_personalizzata) 
```

L'output sarebbe: `Wednesday 25 December 2030`.

## Deep Dive
La funzione `os.date` di Lua è stata introdotta nella versione 5.1 per affrontare la necessità di manipolare le date come stringhe. Esistono alternative, come l'uso delle librerie di terze parti, ma `os.date` è sufficiente nella maggior parte dei casi. Rapido e facile, interpreta la stringa di formato in base ai criteri standard di C, che fornisce sufficiente flessibilità.

## E Anche
- Riferimenti sulla funzione `os.date`: [www.lua.org](https://www.lua.org/pil/22.1.html)
- Tutorial sulla gestione delle date e ore in Lua: [www.tutorialspoint.com](https://www.tutorialspoint.com/lua/lua_date_and_time.htm)
- Spiegazione dettagliata su come `os.date` interpreta il formato: [en.wikibooks.org](https://en.wikibooks.org/wiki/Lua_Programming/os_library#os.date)