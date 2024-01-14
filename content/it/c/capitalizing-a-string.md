---
title:                "C: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Il motivo principale per cui ci si potrebbe impegnare nella programmazione di stringhe nella programmazione C è perché è una funzione fondamentale per la manipolazione dei dati. La capitalizzazione di una stringa è utile per uniformare l'aspetto del testo e può essere necessaria in alcune situazioni specifiche del programma.

## Come fare

Per capitalizzare una stringa in C, è necessario utilizzare la libreria string.h e la funzione "toupper". Di seguito è riportato un esempio di codice che mostra come capitalizzare una stringa di input:

```C
#include <stdio.h> 
#include <string.h> 

int main() { 
  // Definiamo una stringa di input 
  char input[] = "esempio stringa"; 
  
  // Utilizziamo un ciclo for per iterare attraverso la stringa 
  for(int i = 0; i < strlen(input); i++) { 
    // Utilizziamo la funzione toupper per rendere maiuscola ogni singolo carattere 
    input[i] = toupper(input[i]); 
  } 
  
  // Stampiamo la stringa maiuscola 
  printf("Stringa maiuscola: %s", input); 
  
  return 0; 
} 
```

Output: "Stringa maiuscola: ESEMPIO STRINGA"

## Approfondimento

La funzione toupper della libreria string.h è essenziale per la capitalizzazione di una stringa in C. Questa funzione è in grado di convertire qualsiasi carattere minuscolo in maiuscolo. È importante notare che la funzione topper non modifica la stringa originale, ma restituisce una nuova stringa maiuscola, quindi è necessario assegnare il risultato ad una variabile o modificare la stringa originale come mostrato nell'esempio precedente.

Inoltre, è possibile utilizzare la funzione "tolower" per convertire una stringa in minuscolo. Entrambe le funzioni richiedono un parametro di tipo carattere e restituiscono il corrispondente carattere maiuscolo o minuscolo.

## Vedi anche

- [Documentazione ufficiale della libreria string.h in C] (https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Tutorial su come manipolare le stringhe in C] (https://www.programiz.com/c-programming/c-strings)
- [Tutorial su come utilizzare le funzioni toupper e tolower] (https://www.geeksforgeeks.org/c-programming-toupper-toupper/)

Grazie per aver letto! Speriamo che questo articolo ti sia stato utile nella comprensione del processo di capitalizzazione di una stringa in C. Continua a sperimentare con le stringhe e non esitare a consultare la documentazione ufficiale per ulteriori informazioni. Buona programmazione!