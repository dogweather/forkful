---
title:                "C: Trovare la lunghezza di una stringa."
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione essenziale nel mondo della programmazione. Conoscere la lunghezza di una stringa può essere utile per una varietà di scopi, come ad esempio la gestione della memoria o la manipolazione dei dati.

## Come fare

Per trovare la lunghezza di una stringa in C, è possibile utilizzare la funzione `strlen()` presente nella libreria standard `string.h`. Di seguito è riportato un esempio di codice che mostra come utilizzare questa funzione:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char myString[50] = "Ciao a tutti";
  
  // Utilizzo della funzione strlen() per trovare la lunghezza della stringa
  int length = strlen(myString);

  printf("La lunghezza della stringa è: %d", length);

  return 0;
}
```

L'output di questo codice sarà:

```
La lunghezza della stringa è: 12
```

Come si può vedere, la funzione `strlen()` restituisce la lunghezza della stringa passata come parametro. Inoltre, è importante notare che la lunghezza restituita non include il carattere terminatore `\0`.

## Approfondimento

Per coloro che vogliono approfondire l'argomento, è interessante sapere come la funzione `strlen()` funziona internamente. In realtà, questa funzione semplicemente conta il numero di caratteri presenti nella stringa fino a quando non incontra il carattere terminatore `\0`.

La funzione `strlen()` utilizza un puntatore per scorrere la stringa e un contatore per tenere traccia della lunghezza. Ad ogni carattere analizzato, il contatore viene incrementato di 1 fino a raggiungere il carattere terminatore. Questa è anche la ragione per cui la lunghezza della stringa restituita non include il carattere `\0`, poiché il contatore viene incrementato solo dopo aver trovato il carattere terminatore.

È importante notare che la funzione `strlen()` può anche essere utilizzata per trovare la lunghezza di una stringa inserita dall'utente tramite la funzione `fgets()`. In questo caso, il carattere di nuova riga `\n` può essere incluso nella lunghezza restituita e quindi è consigliabile rimuoverlo utilizzando la funzione `strcspn()`.

## Vedi anche

- [Documentazione ufficiale di funzione `strlen()` (in inglese)](https://en.cppreference.com/w/c/string/byte/strlen)
- [Documentazione ufficiale di funzione `fgets()` (in inglese)](https://en.cppreference.com/w/c/io/fgets)
- [Tutorial su stringhe in C (in italiano)](https://www.html.it/pag/134889/le-stringhe-in-c/)