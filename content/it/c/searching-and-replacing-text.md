---
title:                "C: Ricerca e sostituzione di testo"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Il processo di sostituzione del testo è uno dei compiti più comuni quando si lavora con il linguaggio di programmazione C. Ciò può essere necessario per correggere errori di battitura, cambiare nomi di variabili o modificare parti di un programma. Indipendentemente dalla ragione, la capacità di cercare e sostituire il testo può rendere il lavoro di programmazione più efficiente e veloce. In questo articolo, esploreremo come implementare questa funzionalità in C e esploreremo alcune opzioni per migliorarne l'efficacia.

## Come

Per sostituire il testo in C, dobbiamo utilizzare la funzione `strstr()` della libreria di stringhe. Questa funzione cerca una sottostringa all'interno di una stringa principale e restituisce un puntatore alla prima occorrenza trovata. È importante notare che `strstr()` è case-sensitive, quindi le maiuscole e le minuscole devono essere rispettate quando si cerca una corrispondenza.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char text[] = "Questo è un esempio di una stringa";
  char *match;

  match = strstr(text, "esempio"); // cerca "esempio" in "Questo è un esempio di una stringa"

  if (match != NULL) {
  strncpy(match, "cambio", 6);  // sostituisce "esempio" con "cambio"
  }

  printf("%s\n", text); // stampa "Questo è un cambio di una stringa"

  return 0;
}
```

Il codice sopra utilizza la funzione `strstr()` per cercare la parola "esempio" all'interno della stringa `text` e sostituirla con "cambio". Per farlo, utilizziamo anche la funzione `strncpy()` per copiare la nuova parola nella posizione corretta. Nota che dobbiamo specificare la lunghezza della nuova parola nel parametro finale di `strncpy()` per evitare possibili problemi di buffer overflow.

Oltre alla funzione `strstr()`, esistono anche altre funzioni utili per la sostituzione del testo in C, come `strchr()` e `strtok()`. È importante utilizzare la funzione più adatta alle nostre esigenze per ottenere il risultato desiderato.

## Deep Dive

Mentre la sostituzione del testo può sembrare un problema più semplice da risolvere, ci sono alcune considerazioni importanti da tenere a mente per ottenere risultati accurati. Ad esempio, se stiamo lavorando con stringhe di dimensioni variabili, è fondamentale allocare dinamicamente la memoria per evitare errori di allocazione o di overflow. Inoltre, dobbiamo considerare anche i possibili effetti collaterali della sostituzione del testo su altre parti del nostro programma.

Inoltre, dobbiamo tener conto anche delle prestazioni. Se stiamo lavorando con grandi quantità di testo, potrebbe essere più efficiente utilizzare una libreria di espressioni regolari invece delle funzioni di libreria stringa C. Questo ci permette di cercare e sostituire testo in modo più avanzato, utilizzando pattern e wildcard per trovare corrispondenze più complesse.

## Vedi anche

- [Funzione `strstr()` documentazione](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Funzione `strncpy()` documentazione](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Espressioni regolari in C](https://www.regular-expressions.info/c.html)