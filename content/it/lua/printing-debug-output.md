---
title:                "Stampa di output di debug"
html_title:           "Lua: Stampa di output di debug"
simple_title:         "Stampa di output di debug"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Stampare l'output di debug è un modo per visualizzare informazioni utili durante lo sviluppo di un programma in Lua. I programmatori lo fanno per capire cosa sta accadendo all'interno del codice e per individuare eventuali errori.

## Come:

```Lua
-- Esempio di stampa di una variabile
nome = "Mario"
print(nome) -- Stampa "Mario"

-- Esempio di stampa di una tabella
auto = {marca = "Fiat", modello = "500", colore = "rosso"}
print(auto) -- Stampa "{marca="Fiat", modello="500", colore="rosso"}"

-- Esempio di stampa di una stringa con parametri
saluto = "Ciao %s, benvenuto!"
print(saluto:format(nome)) -- Stampa "Ciao Mario, benvenuto!"
```

## Approfondimento:

L'uso di print() è una pratica comune in programmazione e ha origini storiche legate alla stampa su carta dei programmi. Esistono alternative alla stampa di debug in Lua, come ad esempio l'utilizzo di un debugger o la scrittura in un file di log. La funzione print() può essere utilizzata anche per scrivere su stdout, stderr o su una connessione di rete.

## Vedi anche:

- Documentazione ufficiale di Lua: https://www.lua.org/pil/5.2.html
- Articolo su come stampare l'output di debug in Lua: https://www.programming-idioms.org/idiom/131/print-debugging/1487/lua