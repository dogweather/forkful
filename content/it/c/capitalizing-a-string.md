---
title:                "C: Capitalizzazione di una stringa"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

In questo post, parleremo di come capitalizzare una stringa in linguaggio C. Questo può sembrare un argomento banale, ma è un'operazione fondamentale in molti programmi e conoscere come farlo correttamente può semplificare la tua vita da programmatore.

## Come Fare

Per capitalizzare una stringa in linguaggio C, possiamo utilizzare la funzione "toupper" della libreria **ctype.h**. Questa funzione prende un carattere come input e restituisce il carattere equivalente maiuscolo. Possiamo usarlo all'interno di un ciclo for per iterare attraverso ogni carattere nella stringa e utilizzare la funzione "toupper" per convertirlo in maiuscolo.

Ecco un esempio di codice:

```C
#include <stdio.h>
#include <ctype.h>

int main(){
  char string[] = "ciao a tutti";
  int i;

  for(i = 0; string[i]!='\0'; i++){
    string[i] = toupper(string[i]);
  }

  printf("Stringa in maiuscolo: %s", string);
  return 0;
}
```

L'output di questo codice sarà:

```
Stringa in maiuscolo: CIAO A TUTTI
```

## Approfondimento

Per capire meglio come funziona la funzione "toupper", dobbiamo parlare di come i caratteri sono rappresentati in memoria in linguaggio C. I caratteri sono rappresentati come numeri, e la tabella ASCII assegna un numero specifico a ciascun carattere. Per esempio, la lettera "A" equivale al numero 65. 

La funzione "toupper" prende il carattere come input, lo converte in numero e poi aggiunge o sottrae 32 a seconda se il carattere è già maiuscolo o minuscolo. In questo modo, il carattere sarà convertito nel suo equivalente maiuscolo.

## Vedi Anche

- [Libreria Ctype](https://it.wikipedia.org/wiki/Ctype.h)
- [Tabella ASCII](https://it.wikipedia.org/wiki/ASCII)