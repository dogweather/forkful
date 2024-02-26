---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:12.041037-07:00
description: "Convertire una data in una stringa in C comporta la traduzione di una\
  \ struttura di data o timestamp in un formato leggibile dall'uomo. I programmatori\u2026"
lastmod: '2024-02-25T18:49:41.756421-07:00'
model: gpt-4-0125-preview
summary: "Convertire una data in una stringa in C comporta la traduzione di una struttura\
  \ di data o timestamp in un formato leggibile dall'uomo. I programmatori\u2026"
title: Convertire una data in una stringa
---

{{< edit_this_page >}}

## Cosa e Perché?

Convertire una data in una stringa in C comporta la traduzione di una struttura di data o timestamp in un formato leggibile dall'uomo. I programmatori spesso svolgono questo compito per visualizzare le date nei log, nelle interfacce utente, o quando si memorizzano date in un formato basato su testo come JSON o CSV.

## Come fare:

La funzione `strftime` della libreria `<time.h>` è comunemente usata per questo scopo. Ti permette di formattare la data e l'ora in vari modi specificando dei formati. Ecco un esempio veloce:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t ora = time(NULL);
    struct tm *ptm = localtime(&ora);

    // Converte la data e l'ora in una stringa (es., "Mer Giu 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Data e Ora correnti: %s\n", dateStr);
    return 0;
}
```

L'output di esempio potrebbe essere questo:

```
Data e Ora correnti: Mer Giu 30 21:49:08 2021
```

Puoi personalizzare il formato cambiando i formati specificati passati a `strftime`. Per esempio, per ottenere la data nel formato `AAAA-MM-GG`, dovresti usare `"%Y-%m-%d"`.

## Approfondimento

La funzione `strftime` e la libreria `<time.h>` fanno parte della Standard Library di C, che risale allo standard ANSI C originale (C89/C90). Anche se diretto e supportato su molte piattaforme, questo approccio può sembrare di basso livello e ingombrante se confrontato con i linguaggi di programmazione moderni che offrono librerie di date e orari più intuitive.

Si dovrebbe notare, mentre le funzioni di tempo della libreria standard di C sono ampiamente supportate e relativamente semplici da usare, mancano di alcune delle caratteristiche più complesse di manipolazione dei fusi orari e di internazionalizzazione trovate in librerie di linguaggi più nuovi o in librerie C di terze parti come i Componenti Internazionali per Unicode (ICU).

Tuttavia, le capacità di personalizzazione della funzione `strftime` e il suo ampio supporto alle piattaforme la rendono uno strumento affidabile e utile per la conversione di stringhe di date in C. I programmatori provenienti da linguaggi con librerie datetime di alto livello potrebbero dover adattarsi alla sua natura di basso livello, ma la troveranno sorprendentemente potente e versatile per formattare date e orari per una varietà di applicazioni.
