---
title:    "C: Convertire una data in una stringa"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Il processo di convertire una data in una stringa è molto comune nella programmazione, in particolare quando si lavora con date e orari. È importante per garantire che le informazioni siano presentate in un formato facilmente leggibile per gli utenti.

## Come Fare

Per convertire una data in una stringa in linguaggio C, è necessario utilizzare la funzione `strftime ()`. Questa funzione prende in input variabili come la data e l'ora, insieme ad un formato di stringa specifico, e restituisce la data nel formato desiderato. Vediamo un esempio:

```
#include <stdio.h>
#include <time.h>

int main(void) {
  time_t now = time(NULL);
  struct tm *t = localtime(&now);
  
  char str_date[9];
  strftime(str_date, sizeof(str_date), "%d/%m/%Y", t);
  
  printf("Data attuale: %s", str_date);
  
  return 0;
}
```

Output: `Data attuale: 02/09/2021` 

Il codice sopra utilizza le librerie standard `stdio.h` e `time.h`, quindi è necessario includerle nel nostro file. Inoltre, dichiariamo una variabile `time_t` per rappresentare la data e l'ora attuali utilizzando la funzione `time ()`. Con la funzione `localtime ()` possiamo ottenere una struttura `tm` con cui possiamo lavorare. Successivamente, utilizziamo la funzione `strftime ()` e specifica il formato desiderato (`%d/%m/%Y` per il formato GG/MM/AAAA) insieme alla nuova stringa in cui vogliamo che la data venga salvata. Infine, utilizziamo la funzione `printf ()` per stampare la stringa generata sulla console.

## Approfondimento

Esistono molti altri designatori che possono essere utilizzati nella funzione `strftime ()` per ottenere una varietà di formati di data e ora. Alcuni esempi sono `%H` per l'ora in formato 24 ore, `%M` per i minuti e `%S` per i secondi. Inoltre, è possibile combinare più designatori per ottenere un formato personalizzato. Ad esempio, `%d/%m/%Y %H:%M:%S` produrrà un output come `02/09/2021 11:30:45` per la data e l'ora attuali.

Un'altra cosa da tenere in considerazione è che la funzione `strftime ()` restituisce anche la rappresentazione della data e dell'ora in lingua locale. Ad esempio, se l'applicazione viene eseguita in Italia, il formato della data sarà "GG/MM/AAAA" invece di "MM/GG/AAAA".

## Vedi Anche

- [Guida alla funzione strftime in C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Esempi di formato in strftime](https://www.cplusplus.com/reference/ctime/strftime/)