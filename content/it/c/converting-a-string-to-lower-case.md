---
title:                "C: Convertire una stringa in minuscolo"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo, parleremo di come convertire una stringa in minuscolo utilizzando il linguaggio di programmazione C. Questa è una tecnica utile quando si lavora con dati di input o si sta creando un programma che richiede lettere in minuscolo. Continuate a leggere per imparare come farlo!

## Come fare

Per prima cosa, dobbiamo capire cosa intendiamo per "stringa" e "minuscolo" in questo contesto. Una stringa in programmazione è semplicemente una sequenza di caratteri, come ad esempio "ciao" o "1234". Il minuscolo, invece, si riferisce alle lettere dell'alfabeto in minuscolo, come ad esempio "a" o "b".

Per convertire una stringa in minuscolo, utilizzeremo una funzione built-in di C chiamata "tolower()". Vediamo un esempio:

```C
#include <stdio.h>
#include <ctype.h>

int main(){
  char string[] = "CIAO";
  for(int i = 0; i < strlen(string); i++){
    string[i] = tolower(string[i]);
  }
  printf("%s\n", string);
  return 0;
}
```

Questo codice stampa "ciao" come output. Vediamo come funziona:

- La prima riga (#include <stdio.h>) include la libreria standard per l'input/output che ci permette di usare la funzione "printf()". È necessaria per stampare il nostro output.
- La seconda riga (#include <ctype.h>) include la libreria standard per la manipolazione dei caratteri. Ciò ci permette di utilizzare la funzione "tolower()".
- La terza riga dichiara una variabile di tipo "char" (carattere) chiamata "string", con il valore iniziale "CIAO".
- All'interno del ciclo "for", utilizziamo la funzione "tolower()" per convertire ogni carattere della stringa in minuscolo. La funzione restituisce il carattere in minuscolo corrispondente, quindi sostituiamo il carattere originale con quello convertito.
- Infine, stampiamo la stringa convertita utilizzando "printf()" come sempre.

In questo modo, saremo in grado di convertire qualsiasi stringa in minuscolo utilizzando la funzione "tolower()". Assicuratevi di avere la libreria "ctype.h" inclusa nel vostro codice prima di utilizzarla!

## Approfondimento

Cosa succede quando utilizziamo la funzione "tolower()" su una stringa che contiene già caratteri minuscoli? Vediamo un altro esempio:

```C
#include <stdio.h>
#include <ctype.h>

int main(){
  char string[] = "cIAo";
  for(int i = 0; i < strlen(string); i++){
    string[i] = tolower(string[i]);
  }
  printf("%s\n", string);
  return 0;
}
```

Questa volta, l'output sarà ancora "ciao". La funzione "tolower()" converte solo i caratteri iniziali in minuscolo, mentre quelli già in minuscolo rimarranno tali.

Inoltre, potete anche utilizzare la funzione "toupper()" per convertire una stringa in maiuscolo, al posto di "tolower()".

## Vedi anche
- La documentazione ufficiale di C (https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html)
- Un tutorial su come utilizzare le funzioni delle librerie standard di C (https://www.tutorialspoint.com/c_standard_library/index.htm)
- Un esempio di codice che utilizza la funzione "tolower()" (https://www.geeksforgeeks.org/c-program-change-string-lowercase-letters/)
- Una spiegazione più dettagliata su come funziona la funzione "tolower()" (https://www.programiz.com/c-programming/library-function/ctype.h/tolower)