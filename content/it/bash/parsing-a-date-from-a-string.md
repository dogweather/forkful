---
title:                "Analizzando una data da una stringa."
html_title:           "Bash: Analizzando una data da una stringa."
simple_title:         "Analizzando una data da una stringa."
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il parsing di una data da una stringa è un processo comune tra i programmatori che coinvolge l'analisi di una stringa di testo per estrarre informazioni relative a una data specifica. Viene eseguito principalmente per convertire la data in un formato più leggibile e utile per il computer, come ad esempio il formato ISO 8601.

## Come fare:

Per eseguire il parsing di una data da una stringa in Bash, è possibile utilizzare il comando "date" seguito dal formato desiderato tra parentesi quadre. Ad esempio:

```Bash
date +"%Y-%m-%d" -d "12/04/2021"
```

Questo comando restituirà la data "2021-12-04" nel formato ISO 8601. Inoltre, è possibile specificare la lingua desiderata utilizzando l'opzione "-L" e il codice ISO a due lettere della lingua. Ad esempio, per ottenere la data in italiano:

```Bash
date +"%d %b %Y" -d "12/04/2021" -L it
```

Questo restituirà "04 dic 2021". È inoltre possibile passare il risultato a una variabile utilizzando la sintassi "var=$(comando)" per ulteriori operazioni.

## Approfondimento:

Il parsing di una data da una stringa è una pratica molto diffusa nei linguaggi di programmazione, non solo in Bash. Può essere utile nei casi in cui si vuole estrarre una data da un testo più lungo o convertire da un formato ad un altro. Una delle alternative più comuni a Bash per il parsing di una data è l'utilizzo di librerie come "dateutil" in Python o "moment.js" in JavaScript.

Per quanto riguarda l'implementazione, Bash utilizza il programma "date" del sistema operativo per gestire tutti gli aspetti relativi alle date e agli orari. Le opzioni fornite al comando "date" possono variare a seconda delle distribuzioni del sistema operativo.

## Vedi anche:

- Documentazione ufficiale di "date" in Bash: https://linux.die.net/man/1/date 
- Libreria "dateutil" per il parsing di una data in Python: https://dateutil.readthedocs.io/en/stable/ 
- Libreria "moment.js" per il parsing di una data in JavaScript: https://momentjs.com/