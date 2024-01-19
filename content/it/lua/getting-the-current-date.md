---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Ottenere la data corrente è la pratica di visualizzare la data e l'ora attuali. Programmatori lo fanno per registrare i timestamp, programmare eventi futuri, o calcolare la differenza tra le date.

## Come fare:

Ecco un esempio di codice by Lua. Guardatelo sotto:

```Lua
os = require('os')

-- Ottieni la data corrente
print(os.date("*t"))
```
Dopo aver eseguito il codice, l'output sarà simile a questo:

```Lua
{
  year = 2022,
  month = 10,
  day = 30,
  hour = 9,
  min = 34,
  sec = 17,
  isdst = false,  -- Orario legale: attivo o no
  wday = 5,       -- Giorno della settimana (domenica è 1)
  yday = 303,     -- Giorno dell'anno
}
```

## Approfondimento:

1) **Contesto storico:** L'ottenimento della data corrente è fondamentale in Lua da quando è stato introdotto os.date nel 1993. Ha reso semplice per gli sviluppatori la manipolazione e l'ottenimento di informazioni sul data e tempo.

2) **Alternative:** Un'altra funzione legata al tempo in Lua è os.time, che restituisce il tempo corrente espresso in secondi dal 1970. Puoi convertirlo in una data più leggibile usando os.date.

3) **Dettagli di implementazione:** La funzione os.date in Lua utilizza la funzione di libreria C strftime per formattare la data e l'ora, consentendo un alto grado di personalizzazione del formato della data.

## Vedere anche:

- Documentazione Lua os.date: [http://www.lua.org/manual/5.4/manual.html#6.8](http://www.lua.org/manual/5.4/manual.html#6.8)
- Programmare con i tempi e date in Lua: [https://www.tutorialspoint.com/lua/lua_date_and_time.htm](https://www.tutorialspoint.com/lua/lua_date_and_time.htm)
- Funzione strftime per formattare la data in C: [http://www.cplusplus.com/reference/ctime/strftime/](http://www.cplusplus.com/reference/ctime/strftime/)