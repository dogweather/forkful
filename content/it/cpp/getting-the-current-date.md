---
title:                "C++: Ottenerere la data corrente."
simple_title:         "Ottenerere la data corrente."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Perché

La data corrente è un elemento fondamentale in molti programmi, sia per scopi di registrazione che per la gestione di processi. Imparare a ottenere la data corrente è essenziale per essere in grado di sviluppare applicazioni efficienti e funzionali.

## Come Fare

Per ottenere la data corrente in un programma C++, è necessario includere la libreria "ctime". Questa libreria contiene le funzioni necessarie per lavorare con date e orari. L'esempio seguente mostra come ottenere la data corrente e stamparla a schermo:

```C++
#include <iostream>
#include <ctime>

int main () {
    // Otteniamo la data corrente
    time_t now = time(0);

    // Convertiamo la data in una stringa leggibile
    char* data = ctime(&now);

    // Stampiamo la data a schermo
    std::cout << "La data corrente è: " << data << std::endl;

    return 0;
}
```

L'output di questo codice sarà qualcosa del tipo:

```
La data corrente è: Sun Feb 16 12:31:33 2020
```

È importante notare che la data ottenuta è formattata in base alle impostazioni locali del computer, quindi potrebbe essere diversa in base alla lingua o alla posizione geografica.

## Approfondimento

Oltre alla funzione "ctime" mostrata nell'esempio sopra, la libreria "ctime" contiene altre utili funzioni per manipolare date e orari. Ad esempio, la funzione "localtime" permette di ottenere la data e l'orario correnti in base al fuso orario locale. Inoltre, con la funzione "strftime" è possibile formattare la data in modi diversi, specificando una stringa di formato.

Una nota importante da tenere a mente è che le funzioni della libreria "ctime" operano sull'orario del sistema, quindi è importante assicurarsi che l'orario del computer sia corretto per ottenere risultati precisi.

# Vedi Anche

- [Documentazione ufficiale di ctime](https://en.cppreference.com/w/cpp/chrono/c/ctime)
- [Tutorial su come utilizzare le funzioni di data e orario in C++](https://www.learncpp.com/cpp-tutorial/88-date-and-time-part-ii-c-server-side/)
- [Esempi di utilizzo di "getTime" in C++](https://www.geeksforgeeks.org/working-with-date-and-time-in-cpp/)