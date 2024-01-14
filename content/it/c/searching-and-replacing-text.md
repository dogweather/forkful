---
title:                "C: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono uno dei compiti più comuni nella programmazione. Non solo può aiutare a correggere facilmente errori ortografici, ma può anche essere utilizzato per effettuare cambiamenti rapidi ed efficienti all'interno del codice.

## Come fare

Per eseguire una ricerca e sostituzione di testo in C, è necessario utilizzare la funzione tolower () per convertire il testo in minuscolo e la funzione strcmp () per confrontare stringhe di testo. Qui di seguito è riportato un esempio di codice che sostituisce tutte le occorrenze di una lettera con un'altra all'interno di una stringa:

```C
#include <stdio.h>
#include <string.h>

int main()
{
   char testo[] = "Questo è un testo di esempio";
   char lettera1 = 'o';
   char lettera2 = 'a';
   int lunghezza = strlen(testo);

   for(int i = 0; i < lunghezza; i++)
   {
       if (tolower(testo[i]) == lettera1)
       {
           testo[i] = lettera2;
       }
   }

   printf("%s", testo);
   return 0;
}

```

L'output di questo esempio sarà "Questà è un testà di esempie".

## Approfondimento

Esistono diverse varianti di funzioni di ricerca e sostituzione in C, come ad esempio la funzione strcspn () che cerca una stringa di testo per un elenco di caratteri specificati e restituisce il numero di caratteri corrispondenti trovati. Inoltre, è anche possibile effettuare sostituzioni di testo all'interno di file di testo utilizzando la libreria standard di C "stdio.h" e le funzioni fgets () e fputs ().

Una cosa importante da considerare quando si effettuano ricerche e sostituzioni di testo in C è l'efficienza. È buona pratica limitare il numero di chiamate di funzioni di ricerca e sostituzione in modo da non appesantire il codice e rallentare l'esecuzione del programma.

## Vedi anche

- [Funzione tolower](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Funzione strcmp](https://www.tutorialspoint.com/c_standard_library/c_function_strcmp.htm)
- [Funzione strcspn](https://www.tutorialspoint.com/c_standard_library/c_function_strcspn.htm)
- [Libreria standard di C "stdio.h"](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)
- [Funzione fgets](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)
- [Funzione fputs](https://www.tutorialspoint.com/c_standard_library/c_function_fputs.htm)