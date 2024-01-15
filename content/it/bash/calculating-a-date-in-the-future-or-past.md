---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "Bash: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato potrebbe essere utile per pianificare eventi futuri o cercare informazioni su eventi passati. Questa funzione è particolarmente utile per chi lavora con scadenze o studia la storia.

## Come fare
Per calcolare una data nel futuro o nel passato in Bash, è possibile utilizzare il comando `date`. Di seguito un esempio di codice e il corrispondente output:

```Bash
# Calcola la data di oggi
date

# Calcola la data di 5 giorni fa
date -d "5 days ago"

# Calcola la data tra 3 mesi
date -d "3 months"
```

Output:

```
Mer 20 Nov 21:08:47 CET 2019
Gio 11 Nov 21:08:47 CET 2019
Mar 25 Feb 21:08:47 CET 2020
```

## Approfondimento
Per calcolare una data nel futuro o nel passato in Bash, il comando `date` utilizza la stringa di formattazione `%m%d%Y` per indicare a quale data si riferisce il calcolo. È anche possibile utilizzare altre opzioni come `-d` per specificare una data diversa da quella di oggi o `-u` per visualizzare la data in UTC (Tempo Universale Coordinato). Per maggiori informazioni e opzioni avanzate, è possibile consultare il manuale di `date` (digitando `man date` nel terminale).

## Vedi anche
- [Calcolare una data in Python](https://realpython.com/python-datetime/)
- [Scheda di riferimento Bash](https://devhints.io/bash)
- [Guida Bash su Github](https://github.com/Idnan/bash-guide)