---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:20.000396-07:00
description: "Ottenere la data corrente in C comporta l'accesso alla libreria standard\
  \ C per recuperare e formattare la data e l'ora correnti del sistema. I\u2026"
lastmod: '2024-03-11T00:14:17.538814-06:00'
model: gpt-4-0125-preview
summary: "Ottenere la data corrente in C comporta l'accesso alla libreria standard\
  \ C per recuperare e formattare la data e l'ora correnti del sistema. I\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente in C comporta l'accesso alla libreria standard C per recuperare e formattare la data e l'ora correnti del sistema. I programmatori spesso necessitano di questa funzionalità per funzioni di registrazione, marcatura temporale o pianificazione all'interno delle loro applicazioni.

## Come fare:

In C, l'header `<time.h>` fornisce le funzioni e i tipi necessari per lavorare con date e orari. La funzione `time()` recupera l'ora corrente, mentre `localtime()` converte quest'ora nel fuso orario locale. Per visualizzare la data, utilizziamo `strftime()` per formattarla come una stringa.

Ecco un esempio basilare:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Ottieni il tempo corrente
    time(&rawtime);
    // Convertilo in ora locale
    timeinfo = localtime(&rawtime);
    
    // Formatta la data e stampala
    strftime(buffer, 80, "La data di oggi è %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Un esempio di output potrebbe essere il seguente:

```
La data di oggi è 2023-04-12
```

## Approfondimento

La gestione del tempo in C, come facilitata da `<time.h>`, si rifà ai primissimi giorni della lingua e dei sistemi UNIX. È costruita attorno al tipo di dato `time_t`, che rappresenta il tempo corrente come il numero di secondi dall'Epoch Unix (1 gennaio 1970). Sebbene ciò sia efficiente e universalmente compatibile, significa anche che le funzioni temporali della libreria standard C sono intrinsecamente limitate dall'intervallo e dalla risoluzione di `time_t`.

Le applicazioni moderne, specialmente quelle che richiedono timestamp ad alta risoluzione o che trattano date molto lontane nel futuro o nel passato, possono trovare queste limitazioni impegnative. Ad esempio, il problema dell'Anno 2038 è una famosa illustrazione in cui i sistemi che utilizzano un `time_t` a 32-bit andranno in overflow.

Per una gestione del tempo e delle date più complessa, molti programmatori si rivolgono a librerie esterne o alle funzionalità fornite dal sistema operativo. In C++, ad esempio, la libreria `<chrono>` offre capacità di manipolazione del tempo più precise e versatili.

Nonostante le sue limitazioni, la semplicità e l'onnipresenza delle funzioni temporali di C le rendono perfettamente adatte per molte applicazioni. Comprendere questi strumenti è fondamentale per i programmatori C, offrendo una miscela di contesto storico di programmazione e utilità pratica quotidiana.
