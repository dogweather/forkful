---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in C: Una guida rapida

## Cosa e Perché?

Ottenere la data corrente in C significa ottenere le informazioni sulla data e sull'ora del momento. Gli sviluppatori lo fanno per registrare eventi, tracciare il tempo o per funzioni legate alla data, come il calcolo del tempo tra le date.

## Come fare:

Ecco un esempio di come ottenere la data corrente in C:

```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t current_time;
    char* c_time_string;

    current_time = time(NULL);
    c_time_string = ctime(&current_time);

    printf("Current time is %s", c_time_string);

    return 0;
}
```

L'output sarà simile a:

```C
Current time is Fri Jan 1 00:00:00 2030
```

## Approfondimento

Il funzionamento di `time.h` risale ai primi giorni del C. Questa libreria è un modo standardizzato per interagire con l'orologio di sistema ed è supportata su quasi tutte le piattaforme.

Se `time.h` non risponde alle tue esigenze, ci sono molte alternative. Ad esempio, su un sistema Unix-like, potresti usare `sys/time.h` o `gettimeofday()`.

Il modo in cui `time.h` recupera l'ora esatta dipende dal sistema operativo e hardware. Ma in generale, accede all'orologio di sistema del computer e lo converte in un formato utilizzabile.

## Vedi Anche

Se desideri maggiori informazioni su come lavorare con le date e l'ora in C, ti consiglio vivamente di consultare i seguenti collegamenti:

- [Libreria time.h in C](https://www.cplusplus.com/reference/ctime/)
- [Ora del sistema in C](https://en.cppreference.com/w/c/chrono)
- [Funzione time() in C](https://www.geeksforgeeks.org/time-function-in-c/)