---
title:                "C: Convertire una data in una stringa"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perchè convertire una data in una stringa?

Quando si lavora con date in un programma, potrebbe essere necessario convertire la data in una rappresentazione più leggibile come una stringa di caratteri. La conversione da una data formattata a una stringa aiuta a visualizzare le informazioni in modo più comprensibile per l'utente finale.

Ad esempio, se si sta creando un'applicazione di gestione delle prenotazioni per un ristorante, si può voler visualizzare la data della prenotazione come una stringa come "30 Gennaio 2020" invece di "30/01/2020".

## Come fare la conversione?

In linguaggio C, esistono diverse funzioni e librerie per convertire una data in una stringa. Uno dei modi più comuni è utilizzare la funzione `strftime()` che permette di formattare una data in una stringa secondo uno specifico formato.

Ecco un esempio di codice che utilizza `strftime()` per convertire una data nel formato "DD/MM/YYYY" e poi stamparla a schermo utilizzando la funzione `printf()`:

```C
#include <stdio.h>
#include <time.h>

int main(void) {
    // ottieni la data corrente
    time_t now = time(NULL);
    
    // inizializza una struttura tm con la data corrente
    // (standard per la rappresentazione di date e orari)
    struct tm *date = localtime(&now);
    
    // dichiara una stringa di 20 caratteri per contenere la data formattata
    char date_str[20];
    
    // utilizza la funzione strftime per convertire la data in una stringa
    // "%d/%m/%Y" definisce il formato della data (DD/MM/YYYY)
    strftime(date_str, sizeof(date_str), "%d/%m/%Y", date);
    
    // stampa la stringa a schermo
    printf("La data di oggi è: %s", date_str);
    
    return 0;
}
```

Output:

```
La data di oggi è: 30/01/2020
```

## Approfondimenti sulla conversione di una data in una stringa

Mentre la funzione `strftime()` è utile per la maggior parte delle conversioni di date in stringhe, può essere utile conoscere alcune altre funzioni disponibili come `strptime()` per convertire stringhe in date o `asctime()` per ottenere una rappresentazione leggibile della data e dell'ora correnti.

Inoltre, è importante tenere conto dei diversi formati di date accettati dalle funzioni di conversione e assicurarsi che la stringa di output sia abbastanza grande per contenere la data formattata.

## Vedi anche

- [Documentazione ufficiale di strftime()](https://www.cplusplus.com/reference/ctime/strftime/)
- [Formati di data e ora in C](https://www.journaldev.com/32884/c-date-time-format-in-c)
- [Esempi di codice per la conversione di una data in una stringa in C](https://www.techonthenet.com/c_language/standard_library_functions/strftime.php)