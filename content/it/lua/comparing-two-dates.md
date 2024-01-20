---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparare due date in Lua

## Cos'è e perché?

Comparare due date significa verificare quale tra le due date sia precedente, successiva o se sono uguali. Questo è importante per i programmatori quando devono eseguire operazioni basate sulla sequenza temporale delle date.

## Come fare:

Dal momento che Lua utilizza la libreria C standard per gestire le operazioni temporali, possiamo usare il modulo `os` per ottenere il tempo. Vediamo un esempio nel quali si confrontano due date.

```Lua
data1 = os.time({year=2022, month=11, day=21, hour=23, min=20, sec=0})
data2 = os.time({year=2022, month=11, day=21, hour=20, min=30, sec=0})

if data1 > data2 then
   print("La data1 è successiva alla data2")
elseif data1 < data2 then
   print("La data1 è precedente alla data2")
else
   print("Le due date sono uguali")
end
```

Nell'esempio sopra, l'output sarà: “La data1 è successiva alla data2”

## Approfondimento

Storicamente, le date Lua sono basate sul tempo Unix, che parte dal 1 ° gennaio 1970. Ma a differenza del tempo Unix, Lua gestisce anche date e orari futuri oltre il 2038.

Un'alternativa al confronto diretto delle date in Lua potrebbe essere l'uso di librerie di terze parti come `date.lua` che offre funzionalità più robuste per la gestione delle date.

Dettagli di implementazione: quando si utilizza `os.time` si ottiene il numero di secondi trascorsi dal 1° gennaio 1970. Questo facilita il confronto poiché stiamo in pratica paragonando solo due numeri interi.

## Vedi anche
1. [Documentazione ufficiale di Lua](https://www.lua.org/manual/)
2. [Github date.lua](https://github.com/Tieske/date)

Ricorda, è importante conoscere a fondo le date nell'ambito della programmazione. Non sottovalutare mai il loro potenziale!