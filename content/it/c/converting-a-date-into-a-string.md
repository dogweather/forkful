---
title:                "Convertire una data in una stringa"
html_title:           "C: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Converting a date into a string è un'operazione comune nella programmazione e può essere utile in molti scenari diversi. Ad esempio, può essere necessario visualizzare una data in un formato specifico o salvarla in un file di testo.

## Come Fare

Per convertire una data in una stringa in linguaggio C, è necessario utilizzare la funzione `strftime()`. Questa funzione prende tre argomenti: un puntatore a una variabile stringa, la dimensione di quella stringa e un formato specifico per il tipo di dato `time_t` che rappresenta la data.

Per il formato della data, è possibile utilizzare codici di formattazione speciali come `%d` per il giorno, `%m` per il mese e `%Y` per l'anno. Esempio:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t data = time(NULL);
  char stringa[50];
  strftime(stringa, sizeof(stringa), "%d/%m/%Y", localtime(&data));
  printf("La data di oggi è: %s\n", stringa);

  return 0;
}
```

Questa porzione di codice stamperà "La data di oggi è: 04/07/2021".

## Approfondimento

La funzione `strftime()` è in grado di gestire anche formati di data in lingue diverse da quella predefinita nella libreria standard del C. Per farlo, è necessario impostare la variabile `LC_TIME` prima di chiamare la funzione. Ad esempio, per impostare la lingua italiana:

```C
setlocale(LC_TIME, "it_IT");
```

Inoltre, la funzione `strftime()` può essere utilizzata per convertire anche altre informazioni temporali come l'orario o il fuso orario. Per ulteriori dettagli sulle opzioni di formattazione e sulle funzionalità avanzate della funzione, consultare la documentazione ufficiale.

## Vedi Anche

- Documentazione ufficiale `strftime()`: https://en.cppreference.com/w/c/chrono/strftime
- Tutorial su come utilizzare la funzione `strftime()`: https://www.programiz.com/c-programming/library-function/strftime
- Esempi di formattazione delle date e delle ore: https://www.cplusplus.com/reference/ctime/strftime/