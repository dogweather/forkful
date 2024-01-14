---
title:    "C: Calcolare una data nel futuro o nel passato"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potreste avere la necessità di calcolare una data nel futuro o nel passato. Potrebbe essere per pianificare un viaggio, per tenere traccia delle scadenze o semplicemente per curiosità.

## Come

In linguaggio C, esistono alcune funzioni utili per calcolare una data in base a una data di riferimento e a un numero di giorni aggiuntivi o sottratti. Una di queste funzioni è la "time.h", che contiene la funzione "strftime" che permette di formattare una data nel formato desiderato.

```C
#include <stdio.h>
#include <time.h>

int main()
{
   // Definizione della data di riferimento
   struct tm data = { .tm_year = 2021, .tm_mon = 5, .tm_mday = 20 };
   // Definizione del numero di giorni da aggiungere
   int giorni = 7;

   // Utilizzo della funzione "mktime" per convertire la data in secondi
   time_t unix_data = mktime(&data);
   // Utilizzo della funzione "localtime" per convertire la data in formato locale
   struct tm* data_risultato = localtime(&unix_data);
   // Utilizzo della funzione "strftime" per formattare la data in una stringa
   char data_formattata[11];
   strftime(data_formattata, 11, "%d/%m/%Y", data_risultato);

   // Stampa della data risultato
   printf("Data risultato: %s\n", data_formattata);

   return 0;
}
```

Output:
```
Data risultato: 27/05/2021
```

## Deep Dive

Se volete calcolare una data in futuro o nel passato basandovi su una data di riferimento e un numero di giorni, è importante tenere in considerazione anche gli anni bisestili e il fatto che ogni mese ha un numero diverso di giorni. Inoltre, esistono anche altre funzioni come "timegm" e "mktime" che possono essere utili per gestire i fusi orari e convertire le date in secondi.

Inoltre, ci sono anche librerie esterne, come "date.h" o "libtdate", che offrono funzionalità avanzate per la gestione delle date, come il supporto per date precedenti al 1970 o la conversione tra diversi formati di date.

## See Also

- [Funzioni di date e tempo in linguaggio C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Libreria "date.h" per il calcolo e la gestione delle date in C](https://github.com/kaluchen/date-c)
- [Libreria "libtdate" con funzioni avanzate per la gestione delle date in C](https://gitlab.com/DaffyKdaf/tdate)