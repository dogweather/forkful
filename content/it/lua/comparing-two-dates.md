---
title:                "Confronto tra due date"
html_title:           "Lua: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Confrontare due date significa confrontare due diversi momenti nel tempo per verificare se sono uguali o se uno è precedente o successivo all'altro. I programmatori spesso effettuano questo tipo di confronto per gestire flussi di dati temporizzati o per creare funzionalità che dipendono dalla data.

## Come fare:
```Lua
-- Esempio utilizzando una tabella che rappresenta la data {anno, mese, giorno}
-- Confronto di due date utilizzando l'operatore di uguaglianza
local data1 = {2020, 10, 15}
local data2 = {2020, 10, 15}
if data1 == data2 then
  print("Le due date sono uguali.")
else
  print("Le due date sono diverse.")
end
-- Output: Le due date sono uguali.

-- Confronto di due date utilizzando l'operatore maggiore e minore
local data3 = {2019, 12, 31}
local data4 = {2020, 1, 1}
if data3 < data4 then
  print("La prima data è precedente alla seconda.")
elseif data3 > data4 then
  print("La prima data è successiva alla seconda.")
else
  print("Le due date sono uguali.")
end
-- Output: La prima data è precedente alla seconda.
```

## Approfondimento:
Il confronto tra due date può essere utile in diversi contesti, ad esempio nella gestione del calendario nelle lingue di programmazione meno recenti in cui non è presente una funzione specifica per questo scopo. Un'alternativa al confronto diretto potrebbe essere l'utilizzo di una libreria esterna specifica per la manipolazione delle date, che può offrire una maggiore precisione e una sintassi più intuitiva. 
Per quanto riguarda l'implementazione, il confronto è effettuato confrontando l'anno, il mese e il giorno iniziando dal più significativo. Nel caso di un confronto tra timestamp (cifra che rappresenta una data e un orario in secondi dall'Unix epoch), si può utilizzare l'operatore di uguaglianza per verificare l'equivalenza.

## Vedi anche:
- https://www.lua.org/pil/2.5.html (sezione su "Operatori relazionali" del libro "Programming in Lua")
- https://www.lua.org/manual/5.3/manual.html#3.4.6 (sezione sulla "Comparazione di valori" nel manuale di Lua)