---
title:                "Lavorare con csv"
html_title:           "Lua: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con CSV, o Comma-Separated Values, significa manipolare dati organizzati in tabelle di testo con l'uso del separatore virgola. I programmatori utilizzano i CSV per rappresentare e archiviare grandi quantità di dati in modo strutturato e facilmente importabile.

## Come fare:
```Lua 
--Per accedere ai dati contenuti in un file CSV possiamo utilizzare la libreria esterna "lua-csv":
local csv = require("csv")

--Carichiamo il file CSV:
local file = csv.open('esempio.csv')

--Accediamo ai dati del file utilizzando un ciclo for:
for fields in file:lines() do
  printf("Nome: %s, Cognome: %s, Età: %s", fields[1], fields[2], fields[3])
end
```
Output:
```
Nome: Mario, Cognome: Rossi, Età: 35
Nome: Giuseppe, Cognome: Bianchi, Età: 27
Nome: Laura, Cognome: Neri, Età: 40
```

## Approfondimento:
I CSV sono stati introdotti negli anni '70 come uno dei primi formati di file per l'archiviazione di dati. Oggi sono ancora ampiamente utilizzati grazie alla loro semplicità e compatibilità con molti linguaggi di programmazione. Esistono alternative ai CSV come il formato JSON o XML, ma i CSV rimangono una scelta popolare per la loro flessibilità.

## Vedi anche:
Per maggiori informazioni sull'utilizzo dei CSV in Lua, puoi consultare la documentazione della libreria "lua-csv": https://github.com/geoffleyland/lua-csv. Inoltre, puoi leggere ulteriori informazioni sui CSV e sui loro utilizzi nell'articolo di Wikipedia: https://it.wikipedia.org/wiki/CSV_(formato_di_file).