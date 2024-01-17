---
title:                "Eliminare caratteri corrispondenti a un modello."
html_title:           "C: Eliminare caratteri corrispondenti a un modello."
simple_title:         "Eliminare caratteri corrispondenti a un modello."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Cos'è e perché?
Cancellare i caratteri che corrispondono ad un determinato pattern è un'operazione molto comune tra i programmatori. Consiste nel rimuovere tutti i caratteri che soddisfano una determinata condizione, in modo da ottenere un'analisi più accurata dei dati o per semplificare il codice.

# Come fare:
Di seguito sono riportati alcuni esempi di codice in C per mostrare come sia possibile cancellare i caratteri che corrispondono ad un pattern specifico.

```
// questo codice rimuove tutti i caratteri 'a' da una stringa
#include <stdio.h>

void deletePattern(char string[], char pattern) {
  int i, j = 0;
  for(i = 0; string[i] != '\0'; i++) {
    if(string[i] != pattern) {
      string[j++] = string[i];
    }
  }
  string[j] = '\0';
}

int main() {
  char string[] = "ciao a tutti";
  printf("Stringa originale: %s\n", string);
  deletePattern(string, 'a');
  printf("Stringa modificata: %s\n", string);
  return 0;
}

//output:
// Stringa originale: ciao a tutti
// Stringa modificata: cio tutti
```

```
// questo codice rimuove tutti i numeri pari da un array
#include <stdio.h>

void deletePattern(int array[], int size) {
  int i, j = 0;
  for(i = 0; i < size; i++) {
    if(array[i] % 2 != 0) {
      array[j++] = array[i];
    }
  }
}

int main() {
  int array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int size = sizeof(array)/sizeof(array[0]);
  printf("Array originale: ");
  for(int i = 0; i < size; i++) {
    printf("%d ", array[i]);
  }
  deletePattern(array, size);
  printf("\nArray modificato: ");
  for(int i = 0; i < size/2; i++) {
    printf("%d ", array[i]);
  }
  return 0;
}

//output:
// Array originale: 1 2 3 4 5 6 7 8 9 10
// Array modificato: 1 3 5 7 9
```

# Approfondimento:
Cancellare i caratteri corrispondenti ad un determinato pattern è diventata un'operazione molto importante grazie alla crescita delle tecnologie di elaborazione e analisi dati. Prima dell'avvento di linguaggi di programmazione come C, questa operazione richiedeva l'utilizzo di complesse tecniche algoritmiche per manipolare i dati. Oggi è possibile effettuare questa operazione in poche righe di codice, rendendo il lavoro dei programmatori molto più efficiente.

# Vedi anche:
Per maggiori informazioni su come cancellare caratteri corrispondenti ad un pattern in C, puoi consultare la [documentazione ufficiale di C](https://devdocs.io/c/), oppure leggere articoli e tutorial online.