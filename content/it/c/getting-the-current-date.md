---
title:    "C: Ottenere la data corrente"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Spesso nei nostri programmi abbiamo bisogno di conoscere la data attuale. Potrebbe essere per registrare le transazioni, per aggiornare gli eventi futuri o semplicemente per stampare la data su uno schermo. Conoscere la data corrente è fondamentale per molti programmi e in questo post vedremo come ottenerla utilizzando il linguaggio di programmazione C.

## Come fare

Ci sono diverse librerie disponibili per ottenere la data attuale in C, ma la più comune è `time.h`. Includendo questa libreria nel nostro codice, possiamo utilizzare la funzione `time()` per ottenere il numero di secondi trascorsi dal 1 gennaio 1970. Ecco un semplice esempio:

```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t t = time(NULL);
   printf("La data attuale è: %s", ctime(&t));
   return 0;
}
```

In questo esempio, stiamo utilizzando la funzione `ctime()` per convertire il numero di secondi in una stringa leggibile. Il risultato sarà qualcosa del genere:

```
La data attuale è: Thu Jul 22 11:28:44 2021
```

Inoltre, potremmo voler formattare la data in un modo più specifico. Per fare ciò, possiamo utilizzare la funzione `localtime()` per convertire il valore di `time_t` in una struttura `tm`. Ecco un esempio che stampa la data nel formato `yyyy-mm-dd`:

```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t t = time(NULL);
   struct tm* tmptr = localtime(&t);
   printf("La data attuale è: %d-%02d-%02d",
          tmptr->tm_year + 1900,
          tmptr->tm_mon + 1,
          tmptr->tm_mday);
   return 0;
}
```

Il risultato sarà:

```
La data attuale è: 2021-07-22
```

## Approfondimento

Ora che sappiamo come ottenere la data attuale, è importante sapere che questa viene calcolata in base al fuso orario del nostro sistema. Se vogliamo ottenere la data in un determinato fuso orario, possiamo utilizzare la funzione `tzset()` per impostare il fuso orario corretto. Ad esempio, per ottenere la data attuale in fuso orario GMT, possiamo utilizzare il seguente codice:

```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t t = time(NULL);
   tzset(); // impostiamo il fuso orario
   struct tm* tmptr = gmtime(&t);
   printf("La data attuale in fuso orario GMT è: %d-%02d-%02d %02d:%02d:%02d",
          tmptr->tm_year + 1900,
          tmptr->tm_mon + 1,
          tmptr->tm_mday,
          tmptr->tm_hour,
          tmptr->tm_min,
          tmptr->tm_sec);
   return 0;
}
```

Il risultato sarà:

```
La data attuale in fuso orario GMT è: 2021-07-22 09:32:27
```

## Vedi Anche

Ecco alcuni link utili per ulteriori informazioni sulla gestione delle date in C:

- [La libreria time.h su Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [La funzione ctime() su GeeksforGeeks](https://www.geeksforgeeks.org/ctime-function-in-c-cpp/)
- [La funzione localtime() su Programiz](https://www.programiz.com/c-programming/library-function/stdio.h/strftime)
- [La funzione strftime() su Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)