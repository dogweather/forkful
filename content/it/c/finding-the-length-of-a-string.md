---
title:    "C: Trova la lunghezza di una stringa"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione in linguaggio C. Sapere quanto lunga è una stringa è fondamentale per poter manipolare e gestire correttamente i dati all'interno del tuo codice. In questo articolo, ti mostreremo come trovare la lunghezza di una stringa in modo semplice ed efficace.

## Come fare

Per trovare la lunghezza di una stringa in linguaggio C, utilizzeremo la funzione `strlen()` della libreria standard del C. Questa funzione accetta come parametro una stringa e restituisce il numero di caratteri presenti in quella stringa. Ecco un esempio di codice:

```C
#include<stdio.h>
#include<string.h>

int main() {
	char stringa[50] = "Ciao a tutti!";
	int lunghezza = strlen(stringa);
	
	printf("La lunghezza della stringa è: %d", lunghezza);
	
	return 0;
}
```

Output: 
```C
La lunghezza della stringa è: 13
```

Come puoi vedere, abbiamo dichiarato una variabile `lunghezza` di tipo `int` e abbiamo assegnato ad essa il valore restituito dalla funzione `strlen()`. In questo caso, la lunghezza della stringa è di 13 caratteri, inclusi gli spazi.

È importante ricordare che la funzione `strlen()` conta solo i caratteri all'interno della stringa, non considerando il carattere terminatore `'\0'`. Per questo motivo, solitamente si definisce la dimensione dell'array che contiene la stringa con un valore di 1 in più rispetto alla lunghezza effettiva della stringa.

## Approfondimento

Ora che hai imparato come trovare la lunghezza di una stringa in C, è importante capire come questa operazione viene eseguita. La funzione `strlen()` itera attraverso tutti i caratteri della stringa, contando quanti di essi sono validi. Quando raggiunge il carattere terminatore `'\0'`, interrompe il conteggio e restituisce il numero di caratteri contati.

In alcuni casi, potresti incontrare un problema noto come "buffer overflow", ovvero quando la lunghezza fornita alla funzione `strlen()` è maggiore della dimensione dell'array che contiene la stringa. Questo può causare un errore imprevisto o addirittura una vulnerabilità di sicurezza nel tuo programma. Per evitare questo problema, è importante prestare attenzione alla dimensione dell'array durante la sua inizializzazione.

## Vedi anche

- [Documentazione ufficiale della funzione `strlen()`](https://en.wikipedia.org/wiki/C_string_handling#strlen)
- [Tutorial su come gestire correttamente le stringhe in linguaggio C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Altri esempi di codice per trovare la lunghezza di una stringa](https://www.includehelp.com/c-programming-examples/find-length-of-a-given-string.aspx)