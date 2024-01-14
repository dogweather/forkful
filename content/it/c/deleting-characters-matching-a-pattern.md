---
title:                "C: Cancellazione di caratteri corrispondenti a un modello."
simple_title:         "Cancellazione di caratteri corrispondenti a un modello."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per voler eliminare caratteri corrispondenti a un determinato pattern in un programma C. Potresti voler pulire i dati di input o rimuovere informazioni sensibili prima di salvarle o inviarle. Inoltre, l'eliminazione di caratteri in questo modo può essere utile per validare l'input dell'utente o per formattare correttamente i dati per un certo scopo.

## Come eseguire l'operazione

Per eliminare caratteri in base a un pattern, possiamo utilizzare la funzione "strchr" della libreria standard di C. Questa funzione accetta due argomenti: una stringa e un carattere. Restituisce un puntatore alla prima occorrenza del carattere nella stringa, se presente, o NULL se il carattere non è stato trovato.

```C
#include <stdio.h>
#include <string.h>

int main() {
  // Dichiarazione di una stringa di esempio
  char stringa[] = "Hello World!";
  // Dichiarazione di un carattere da eliminare
  char carattere = 'o';

  // Utilizzo della funzione "strchr" per trovare la prima occorrenza del carattere nella stringa
  char *ptr = strchr(stringa, carattere);

  // Continua ad eliminare tutti i caratteri corrispondenti fino a quando non trovi più il carattere nella stringa
  while (ptr != NULL) {
    // Sostituisci il carattere con uno spazio vuoto
    *ptr = ' ';
    // Utilizzo di "strchr" per trovare una nuova occorrenza del carattere
    ptr = strchr(stringa, carattere);
  }

  // Stampa il risultato della stringa modificata
  printf("%s\n", stringa);

  return 0;
}
```

Output:

```
Hell  W rld!
```

## Approfondimento

La funzione "strchr" utilizza un approccio semplice ma efficace basato su un ciclo while per scorrere la stringa e sostituire ogni occorrenza del carattere corrispondente con uno spazio vuoto. Tuttavia, ci sono altre opzioni per eliminare caratteri in base a un pattern come l'utilizzo della funzione "strtok" o l'utilizzo di espressioni regolari.

## Vedi anche

- Documentazione ufficiale su "strchr": https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm
- Guida su come utilizzare la funzione "strtok": https://www.geeksforgeeks.org/strtok-strtok_r-functions-c-examples/
- Tutorial sull'utilizzo delle espressioni regolari in C: https://www.regular-expressions.info/libc.html