---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Parserizzare una data da una stringa appunta a convertire un testo che rappresenta una data in un formato comprensibile per il codice. I programmatori fanno questo per poter manipolare e utilizzare le date nei loro programmi in modo efficiente.

## Come fare:
In Fish Shell, possiamo parserizzare una data da una stringa usando il comando 'date':

```Fish Shell
set data_stringa "2022-03-27 09:00:00"

# Convertire la stringa a data
set data (date -d $data_stringa)

echo $data
```

Uscita prevista:

```Fish Shell
Dom 27 Mar 2022 09:00:00 CET
```

## Approfondire
Questo conveniente metodo per interpretare le date ha una lunga storia nel comando UNIX 'date', che risale agli albori del sistema operativo Unix negli anni '70. 
Firefox e Chrome, per esempio, hanno implementato metodi alternativi per il parsing delle date con le loro rispettive funzioni Date.parse(), ma il comando 'date' continua ad essere utilizzato per la sua semplicità e compatibilità con una vasta gamma di sistemi. 

A livello di implementazione, 'date' legge la stringa da sinistra a destra, cercando di abbinare i modelli dei formati di data comuni. Quando trova un match, converte la corrispondenza nel corrispondente valore di data. 

## Vedi anche
- Manuale di Fish Shell: https://fishshell.com/docs/current/commands.html
- Uso del comando 'date' in Linux: https://www.lifewire.com/date-command-in-linux-4090726
- Ulteriori informazioni sui metodi Date.parse() di JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse