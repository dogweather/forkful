---
aliases:
- /it/c/comparing-two-dates/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:26.182753-07:00
description: "Comparare due date in C comporta la determinazione della relazione cronologica\
  \ tra di loro - se una data precede l'altra o se sono uguali. Questa capacit\xE0\
  \u2026"
lastmod: 2024-02-18 23:08:56.351596
model: gpt-4-0125-preview
summary: "Comparare due date in C comporta la determinazione della relazione cronologica\
  \ tra di loro - se una data precede l'altra o se sono uguali. Questa capacit\xE0\
  \u2026"
title: Confrontare due date
---

{{< edit_this_page >}}

## Cosa & Perché?

Comparare due date in C comporta la determinazione della relazione cronologica tra di loro - se una data precede l'altra o se sono uguali. Questa capacità è cruciale in applicazioni che gestiscono pianificazioni, scadenze o registrazioni, poiché consente l'organizzazione e la manipolazione di dati sensibili al tempo.

## Come fare:

C non dispone di un tipo integrato per le date, rendendo necessario l'utilizzo della libreria `time.h` per lavorare con le strutture di data e ora. La struttura `tm` e la funzione `difftime()` sono comunemente usate per comparare le date. Qui sotto è riportato un esempio che mostra come comparare due date:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double secondi;

    // Prima data (AAAA, MM, GG)
    date1.tm_year = 2023 - 1900; // Anno dal 1900
    date1.tm_mon = 3 - 1;        // Mese [0-11]
    date1.tm_mday = 15;          // Giorno del mese [1-31]

    // Seconda data (AAAA, MM, GG)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Convertire in formato time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Comparare
    secondi = difftime(time1, time2);

    if (secondi == 0) {
        printf("Le date sono uguali.\n");
    } else if (secondi > 0) {
        printf("La prima data viene dopo la seconda.\n");
    } else {
        printf("La prima data viene prima della seconda.\n");
    }

    return 0;
}
```

L'output potrebbe essere:

```text
La prima data viene prima della seconda.
```

Questo programma inizializza due strutture `tm` con date specifiche, le converte nel formato `time_t` utilizzando `mktime()`, e infine le compara usando `difftime()`, che restituisce la differenza in secondi (come un `double`) tra i due tempi.

## Approfondimento

Nei primi giorni del linguaggio C, le operazioni con date e orari richiedevano calcoli manuali, spesso tenendo conto degli anni bisestili, del numero variabile di giorni nei mesi e persino dei secondi intercalari. L'introduzione di `time.h` nello standard ANSI C ha portato alla standardizzazione della gestione del tempo in C, semplificando le operazioni con date e orari.

Usare `time.h` per la comparazione di date è semplice ma ha delle limitazioni. La struttura `tm` non considera i fusi orari o l'ora legale, e `difftime()` fornisce solo la differenza in secondi, mancando di una granularità più fine per certe applicazioni.

Per le applicazioni che richiedono operazioni di data-ora più robuste, inclusi il supporto per i fusi orari, le transizioni dell'ora legale, e intervalli di tempo più precisi, librerie come `date.h` (una libreria di date di Howard Hinnant, non parte della libreria standard) offrono un'alternativa moderna a `time.h`. Queste librerie forniscono strumenti più completi per la manipolazione di date-ora in C++, beneficiando di decenni di evoluzione nella progettazione dei linguaggi di programmazione. Per i programmatori C, è necessario utilizzare queste librerie esterne o gestire meticolosamente direttamente le complessità dei calcoli di data-ora per ottenere una manipolazione di date-ora precisa e culturalmente consapevole.
