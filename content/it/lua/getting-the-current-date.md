---
title:                "Ottener la data corrente"
html_title:           "Lua: Ottener la data corrente"
simple_title:         "Ottener la data corrente"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente in programmazione significa ottenere l'ora e la data attuali sulla macchina in cui il codice viene eseguito. Questo è spesso utile per la gestione delle scadenze, la cronologia delle azioni degli utenti e altre operazioni legate al tempo. I programmatori spesso usano questa funzione per tenere traccia del tempo nel loro codice.

## Come fare:

```Lua
-- Usando la funzione os.date()
local data_corrente = os.date()
print(data_corrente)
-- Output: Gio 29 Lug 2021 19:44:00 CEST

-- Specificando un formato personalizzato
local data_corrente = os.date("%c")
print(data_corrente)
-- Output: Gio 29 Lug 2021 19:44:00

-- Ottenere solo la data corrente
local data_corrente = os.date("%x")
print(data_corrente)
-- Output: 07/29/21
```

## Approfondimento:

Ottenere la data corrente ha avuto un'altra rilevanza nel passato quando i computer non avevano un hardware interno per tenere traccia del tempo. Invece, la data corrente doveva essere inserita manualmente ogni volta che si accendeva il computer. Ora, i computer moderni hanno un orologio interno che traccia la data e l'ora in tempo reale.

Un'alternativa ad ottenere la data corrente in Lua è utilizzare l'API C per la gestione del tempo. Questo può essere utile per eseguire operazioni più complesse legate al tempo, ma richiede una maggiore comprensione della programmazione a basso livello.

Per quanto riguarda l'implementazione, la funzione os.date() utilizza l'orologio interno del sistema operativo per ottenere la data e l'ora attuali. Il formato specificato nella funzione può variare a seconda del sistema operativo in uso.

## Vedi anche:

- [Lua Reference Manual](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Managing Time in Lua](https://www.tecgraf.puc-rio.br/~lhf/ftp/doc/lua/sources/cat-time.html)