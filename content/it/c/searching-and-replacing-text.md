---
title:    "C: Cercare e sostituire il testo"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un programma, è spesso necessario apportare modifiche al testo che viene visualizzato dall'utente. Una delle operazioni più comuni è la ricerca e la sostituzione di una determinata porzione di testo con un'altra. Questo può sembrare un compito banale, ma in realtà può risparmiare molto tempo e rendere il codice più efficiente.

## Come

In linguaggio C, esistono diverse funzioni che permettono di cercare e sostituire il testo all'interno di una stringa. La funzione più comune è `strchr()`, che cerca un carattere specifico all'interno della stringa e restituisce un puntatore alla sua posizione. Ad esempio, il seguente codice cerca il carattere 'a' all'interno della stringa "ciao mondo" e lo sostituisce con 'e':

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "ciao mondo";
  char *ptr = strchr(str, 'a');
  *ptr = 'e';
  printf("%s\n", str);
  return 0;
}
```

L'output sarà "cieo mondo". In questo modo, abbiamo sostituito il primo carattere 'a' trovato con 'e'.

## Approfondimento

Oltre alla funzione `strchr()`, esistono altre funzioni utili per la ricerca e la sostituzione di testo in C, come `strrchr()` che cerca l'ultima occorrenza di un carattere all'interno di una stringa, o `strstr()` che cerca un sottostringa all'interno di una stringa. Inoltre, è possibile utilizzare anche la libreria "regex.h", che offre maggiori opzioni per la ricerca e la sostituzione di testo utilizzando espressioni regolari.

## Vedi anche

- [Funzioni stringa in C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Libreria regex.h](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)