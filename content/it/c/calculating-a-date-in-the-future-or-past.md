---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "C: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per diverse ragioni, ad esempio per la pianificazione di eventi o per la gestione di dati temporalmente correlati.

## Come Fare

Per calcolare una data nel futuro o nel passato in C, è necessario utilizzare le funzioni `time()` e `localtime()`. Ecco un esempio di codice che calcola la data di oggi più 10 giorni:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL); // ottiene l'attuale tempo in formato Epoch (secondi trascorsi dal 1 gennaio 1970)
    struct tm *today = localtime(&t); // converte il tempo Epoch in una struttura tm contenente le informazioni sulla data e sull'ora attuali
    today->tm_mday += 10; // aggiunge 10 giorni alla data attuale
    mktime(today); // normalizza la data, ad esempio se la data risultante è il 32° giorno del mese, lo converte al 1° del mese successivo
    printf("La data di oggi più 10 giorni è: %d/%d/%d\n", today->tm_mday, today->tm_mon + 1, today->tm_year + 1900); // %d rappresenta un numero intero, %d/%d/%d stampa la data in formato giorno/mese/anno
    return 0;
}
```

**Output:**

```
La data di oggi più 10 giorni è: 23/3/2021
```

Per calcolare una data nel passato, basta modificare il numero di giorni aggiunti (o sottratti) alla data attuale.

## Approfondimento

In C, le date sono gestite utilizzando la struttura `tm` che contiene informazioni sulla data e sull'ora, come giorno, mese, anno, ora, minuti, secondi e altro. La funzione `time()` restituisce il tempo attuale in formato Epoch e la funzione `localtime()` converte questo tempo in una struttura `tm`. Utilizzando le funzioni `mktime()` e `localtime()`, la data ottenuta può essere normalizzata, garantendo che sia una data valida. È possibile utilizzare questi stessi principi per calcolare una data in un altro formato, ad esempio aggiungendo mesi o anni anziché giorni.

## Vedere Anche

- C Time and Date Library: http://www.cplusplus.com/reference/ctime/
- Epoch Converter: https://www.epochconverter.com/