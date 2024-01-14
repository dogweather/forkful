---
title:    "C: Calcolare la lunghezza di una stringa"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso in programmazione ci troviamo ad avere la necessità di conoscere la lunghezza di una stringa di testo. Questa informazione è utile per molteplici scopi, come ad esempio la gestione dei dati provenienti da input dell'utente o il controllo di dimensioni massime nelle operazioni di concatenazione di stringhe.

## Come fare

Esistono diverse funzioni in linguaggio C per calcolare la lunghezza di una stringa. Iniziamo con la più comune: `strlen()`. Questa funzione richiede come parametro la stringa di cui si vuole conoscere la lunghezza e restituisce un valore intero corrispondente alla quantità di caratteri presenti nella stringa. Vediamo un esempio di utilizzo:

```C
#include <stdio.h>
#include <string.h>

int main()
{
  char stringa[] = "Ciao Mondo!";
  int lunghezza = strlen(stringa);
  printf("La lunghezza della stringa è: %d", lunghezza);
  return 0;
}
```

Output:

```
La lunghezza della stringa è: 11
```

Oltre a `strlen()`, esistono anche altre funzioni come `strnlen()` e `wcslen()` che permettono di specificare una lunghezza massima da contare o che possono essere utilizzate con stringhe di tipo wide (con caratteri unicode). Esempi d'utilizzo di queste funzioni possono essere trovati nella documentazione del linguaggio.

## Approfondimento

Per comprendere meglio come queste funzioni vengano implementate, è utile anche comprendere come viene rappresentata una stringa in memoria. In linguaggio C, una stringa è semplicemente un array di caratteri terminato dal carattere nullo `'\0'`. Quando una funzione come `strlen()` viene chiamata, essa inizia a contare i caratteri dalla posizione di memoria in cui si trova la stringa fino a incontrare il carattere nullo di terminazione. È importante ricordare che il valore restituito da queste funzioni include anche il carattere nullo.

## Vedi anche

- Documentazione ufficiale del linguaggio C su stringhe: https://www.tutorialspoint.com/cprogramming/c_strings.htm
- Funzioni utili per la gestione di stringhe in C: https://www.programiz.com/c-programming/c-strings-functions