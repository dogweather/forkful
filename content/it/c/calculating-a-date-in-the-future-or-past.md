---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "C: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Calcolare una data nel futuro o nel passato è un'operazione comune in programmazione quando è necessario gestire eventi temporali o pianificare attività. I programmatori spesso usano questa funzione per determinare scadenze, pianificare azioni ripetitive o gestire i turni di lavoro.

## Come fare:

Utilizzando il linguaggio di programmazione C, è possibile calcolare una data specificando una data di riferimento e un numero di giorni da aggiungere o sottrarre. Ad esempio, per ottenere la data che cade tra 30 giorni a partire da oggi, si può utilizzare la seguente sintassi:

```C
#include <stdio.h>
#include <time.h>

int main()
{
  // imposto una data di riferimento come oggi
  time_t oggi = time(NULL);
  // calcolo il numero di secondi in 30 giorni
  int giorni = 30 * 24 * 60 * 60; 
  // aggiungo i secondi alla data di riferimento
  time_t data_nel_futuro = oggi + giorni;
  // stampo la data nel futuro
  printf("La data tra 30 giorni sarà %s\n", ctime(&data_nel_futuro));
  return 0;
}
```

L'output sarà una stringa che rappresenta la data nel futuro, come ad esempio:

```
La data tra 30 giorni sarà Mon Oct 11 13:33:37 2021
```

## Approfondimento:

La gestione delle date nei sistemi informatici ha una lunga storia e possono esserci diverse alternative per calcolare una data nel futuro o nel passato. Alcune librerie di terze parti offrono funzionalità più avanzate e permettono di manipolare le date in modi diversi. Inoltre, è importante considerare il fuso orario e i vari formati di rappresentazione delle date in diverse lingue e nazionalità.

## Vedi anche:

- [La libreria di sistema time.h](https://en.wikipedia.org/wiki/Time.h)
- [Funzioni matematiche di base in C](https://it.wikipedia.org/wiki/stdlib.h)
- [Alternaive a C per la gestione delle date](https://www.geeksforgeeks.org/alternatives-for-time-function-in-c-c/)