---
title:    "C: Ottenere la data corrente"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

### Perché

A volte nei nostri programmi dobbiamo essere in grado di ottenere la data corrente. Potrebbe essere per registrare quando un'operazione è stata eseguita, per mostrare la data in un formato specifico o per altre finalità. In questo articolo vedremo come possiamo ottenere la data corrente in un programma C.

### Come

Per ottenere la data corrente in un programma C, dobbiamo utilizzare la libreria standard <time.h>. Questa libreria contiene funzioni utili per lavorare con il tempo e la data. Uno dei modi più semplici per ottenere la data corrente è utilizzare la funzione time(), che ci restituisce il numero di secondi trascorsi dal 1 gennaio 1970. Ecco un esempio di codice:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Otteniamo la data corrente
    time_t current_time = time(NULL);

    // La convertiamo in una stringa
    char* date_string = ctime(&current_time);

    // Stampiamo la data corrente
    printf("Data corrente: %s\n", date_string);

    return 0;
}
```

L'output di questo programma sarà qualcosa del genere:

```
Data corrente: Mon Nov 1 12:00:00 2021
```

Possiamo anche formattare la data in modi diversi utilizzando la funzione strftime() che ci permette di specificare il formato che vogliamo ottenere. Ecco un esempio modificato del codice precedente:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Otteniamo la data corrente
    time_t current_time = time(NULL);

    // Definiamo un buffer per la data formattata
    char buffer[80];

    // Formattiamo la data
    strftime(buffer, 80, "%A %d %B %Y", localtime(&current_time));

    // Stampiamo la data formattata
    printf("Data corrente: %s\n", buffer);

    return 0;
}
```

L'output di questo programma sarà qualcosa del genere:

```
Data corrente: lunedì 1 novembre 2021
```

### Deep Dive

Oltre alle funzioni che abbiamo visto in precedenza, ci sono altre opzioni per ottenere la data corrente in un programma C. Ad esempio, possiamo utilizzare la struct tm che rappresenta il tempo e la data e ci permette di accedere a ogni componente (anno, mese, giorno, ora, ecc.). Inoltre, possiamo utilizzare la funzione localtime() per ottenere il tempo locale anziché quello UTC.

Inoltre, è importante notare che la funzione time() restituisce un valore di tipo time_t, che è semplicemente un long int. Quindi, ci serve una qualche rappresentazione leggibile dalla data corrente. È qui che vengono in aiuto le funzioni ctime() e strftime() che convertono il valore time_t in una stringa leggibile.

### Vedi anche

- [Documentazione della libreria <time.h>](https://www.gnu.org/software/libc/manual/html_node/Time-and-Date-Basics.html)
- [Spiegazione della funzione strftime()](https://en.wikipedia.org/wiki/Strftime)
- [Esempi pratici di utilizzo delle funzioni per ottenere la data corrente in C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)