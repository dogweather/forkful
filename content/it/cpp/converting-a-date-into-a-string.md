---
title:                "C++: Convertire una data in una stringa"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui potresti avere la necessità di convertire una data in una stringa all'interno di un programma in C++. Forse stai lavorando con dati da un database e hai bisogno di un modo per rappresentare le date come testo leggibile. Oppure potresti voler creare un'applicazione che visualizzi le date in formati personalizzati per gli utenti. In ogni caso, sapere come convertire una data in una stringa può essere un'abilità utile per i programmatori.

## Come Fare

Per convertire una data in una stringa in C++, è necessario utilizzare la funzione `strftime()` della libreria `<ctime>`. Ecco un esempio di codice che mostra come utilizzare questa funzione per convertire una data nel formato "DD/MM/YYYY":

```C++
#include <iostream>
#include <ctime>

int main() {
    // Definiamo una data come una struttura "tm"
    std::tm data = {0, 0, 0, 20, 8, 2021 - 1900};

    // Creiamo una stringa di output con una dimensione sufficiente per contenere la data formattata
    char output[11];

    // Utilizziamo la funzione strftime per formattare la data in una stringa
    strftime(output, sizeof(output), "%d/%m/%Y", &data);

    // Stampiamo la data formattata
    std::cout << output << std::endl;

    return 0;
}
```

Output:
```
20/08/2021
```

Ci sono molti parametri che è possibile utilizzare con `strftime()` per ottenere formati di data diversi. Ad esempio, `%d` rappresenta il giorno del mese con due cifre, `%m` rappresenta il mese con due cifre e `%Y` rappresenta l'anno con quattro cifre. Per maggiori informazioni su questi parametri e su come utilizzarli, si consiglia di consultare la documentazione ufficiale della funzione `strftime()`.

## Approfondimenti

La funzione `strftime()` può anche essere utilizzata per ottenere altre informazioni sulla data, come ad esempio il giorno della settimana o il numero della settimana dell'anno. Inoltre, è possibile utilizzare la libreria `<chrono>` per ottenere maggiori informazioni sugli intervalli di tempo e sulle date.

## Vedi Anche

- [Documentazione ufficiale della funzione `strftime()`](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Documentazione ufficiale della libreria `<chrono>`](https://en.cppreference.com/w/cpp/chrono)