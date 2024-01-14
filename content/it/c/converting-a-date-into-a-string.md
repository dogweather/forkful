---
title:                "C: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C che si trova a dover manipolare le date, potresti trovarti in situazioni in cui devi convertire una data in una stringa. Questo può essere utile quando si lavora con strutture dati come database o file di testo che richiedono una stringa per rappresentare una data.

## Come fare

Per convertire una data in una stringa in linguaggio C, è possibile utilizzare la funzione `strftime ()` dalla libreria `time.h`. Questa funzione accetta tre parametri: un buffer per contenere la stringa risultante, una dimensione per questo buffer e un formato di data specificato dai caratteri di controllo.

```
#include <stdio.h>
#include <time.h>

int main() {
    // Ottenere la data corrente
    time_t now = time(NULL);

    // Creare un buffer per la stringa risultante
    char date_string[20];

    // Convertire la data in una stringa
    strftime(date_string, sizeof(date_string), "%d-%m-%Y", localtime(&now));

    // Stampare la stringa
    printf("La data corrente è: %s\n", date_string);

    return 0;
}
```

Output:

```
La data corrente è: 11-10-2021
```

Il parametro del formato può essere modificato per ottenere una stringa con diversi formati di data come: `%Y-%m-%d` per il formato AAAA-MM-GG o `%B %d, %Y` per il formato mese giorno, anno (es. ottobre 11, 2021).

## Approfondimento

Durante la conversione di una data in una stringa in C, è importante tener presente che la funzione `strftime ()` utilizza la struttura dati `tm` per rappresentare una data. Questa struttura ha diversi campi per la rappresentazione di un'informazione specifica come il giorno, il mese, l'anno, l'ora, il minuto e così via. Quindi, quando si specifica il formato di data nella funzione `strftime ()`, è possibile accedere a questi campi utilizzando caratteri di controllo come nel seguente esempio:

```
strftime(date_string, sizeof(date_string), "Sono le %I:%M %p del %d %B %Y.", localtime(&now));
```

Output:

```
Sono le 11:50 AM del 11 ottobre 2021.
```

Inoltre, se hai bisogno di convertire una data in una stringa utilizzando la lingua italiana, è possibile specificare il parametro `locale` nella funzione `setlocale ()` per impostare le impostazioni regionali corrette.

## Vedi anche

- Documentazione ufficiale della funzione `strftime ()`: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html
- Tutorial sulla manipolazione delle date in C: https://www.cprogramming.com/tutorial/time.html
- Esempi di caratteri di controllo per il formato della data: https://www.cplusplus.com/reference/ctime/strftime/