---
title:                "Ottenere la data corrente"
aliases: - /it/lua/getting-the-current-date.md
date:                  2024-02-03T19:10:10.154747-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Il recupero della data corrente nella programmazione è un compito cruciale per una moltitudine di applicazioni, inclusi i registri degli eventi, la marcatura temporale (timestamping) degli eventi o la pianificazione delle attività. In Lua, questa funzionalità consente ai programmatori di gestire operazioni relative a data e ora senza problemi all'interno delle loro applicazioni, garantendo che il loro software possa interagire efficacemente con dati in tempo reale.

## Come fare:

Lua offre la funzione `os.date` per ottenere la data e l’ora correnti. La funzione può essere utilizzata senza argomenti per ottenere una stringa formattata o con specificatori di formato per personalizzare l'output. Ecco come usarla:

```lua
-- Ottenere la data e l'ora correnti come stringa formattata
print(os.date())  -- es., Thu Mar  3 14:02:03 2022

-- Personalizzare il formato dell'output
-- %Y per l'anno, %m per il mese, %d per il giorno, %H per l'ora, %M per i minuti
print(os.date("%Y-%m-%d %H:%M"))  -- es., 2022-03-03 14:02
```

Per manipolazioni più sofisticate di date e orari, Lua non dispone di librerie integrate tanto ricche quanto alcuni altri linguaggi di programmazione. Tuttavia, è possibile utilizzare librerie di terze parti come `lua-date` (https://github.com/Tieske/date). Questa libreria offre funzionalità più complete per la manipolazione di date e orari. Ecco come potresti usarla:

Prima, assicurati di avere installato la libreria `lua-date`. Tipicamente puoi installarla usando LuaRocks con il seguente comando:

```bash
luarocks install lua-date
```

Poi, puoi usarla nel tuo script Lua in questo modo:

```lua
local date = require("date")

-- Creare un oggetto data per la data e l'ora correnti
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- es., 2022-03-03 14:02:03
```

Questo esempio dimostra la creazione di un oggetto `date` che rappresenta il momento corrente, che poi puoi formattare in modo simile alla funzione `os.date` ma con flessibilità e opzioni aggiunte fornite dalla libreria `lua-date`.
