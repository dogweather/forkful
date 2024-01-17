---
title:                "Ottenere la data corrente."
html_title:           "C: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente è una pratica comune nella programmazione per recuperare l'informazione su quale giorno, mese e anno è. I programmatori spesso utilizzano questa informazione per gestire gli eventi in base alla data, per generare istantanee dei dati o per scopi di debug.

## Come fare:

Per ottenere la data corrente in linguaggio C, si può utilizzare la funzione ```time``` dalla libreria ```<time.h>```. Questa funzione restituisce il numero di secondi trascorsi dal 1 gennaio 1970 (conosciuto come "epoch time"). Ecco un esempio di codice:

```
#include <stdio.h>
#include <time.h>

int main() {
   time_t now;
   time(&now);

   printf("La data corrente è: %s", ctime(&now));

   return 0;
}
```

E il relativo output:

```
La data corrente è: Sun Aug 15 15:13:29 2021```

## Approfondimento:

La funzione ```time``` è stata introdotta nella libreria standard di C nel 1970 ed è ampiamente utilizzata dai programmatori per ottenere la data corrente. Tuttavia, ci sono anche altre varie opzioni per recuperare la data, come ad esempio utilizzare funzioni specifiche del sistema operativo o librerie di terze parti.

Per ottenere la data corrente in modo più specifico, è possibile utilizzare la funzione ```localtime``` per convertire il numero di secondi restituito dalla funzione ```time``` in una struttura ```tm``` con i dati sulla data e l'ora. Ad esempio:

```
#include <stdio.h>
#include <time.h>

int main() {
   time_t now;
   time(&now);

   struct tm *date = localtime(&now);

   printf("Oggi è il %d/%d/%d ", date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);
   printf("alle %d:%02d:%02d", date->tm_hour, date->tm_min, date->tm_sec);

   return 0;
}
```

E il relativo output:

```Oggi è il 15/08/2021 alle 15:13:29```

## Vedi anche:

- [GNU C Library documentation for time functions](https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html)
- [Date and time functions in C](https://www.programiz.com/c-programming/library-function/time)
- [How to get the current date and time in C](https://www.techonthenet.com/c_language/standard_library_functions/time_h/localtime.php)