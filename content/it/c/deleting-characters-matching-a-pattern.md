---
title:    "C: Cancellazione di caratteri corrispondenti a un modello"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

La cancellazione dei caratteri corrispondenti a un modello può essere utile quando si sta cercando di manipolare una stringa o un file di testo. Ad esempio, potresti voler eliminare tutti i caratteri non numerici da una stringa di input per poter eseguire calcoli matematici su di essa.

## Come fare

Per eliminare i caratteri corrispondenti a un modello utilizzando il linguaggio di programmazione C, è possibile utilizzare la funzione "strpbrk" della libreria standard di C. Questa funzione restituisce un puntatore alla prima occorrenza di una qualsiasi delle lettere specificate all'interno della stringa. Utilizzando questo puntatore, è possibile ciclare attraverso la stringa e rimuovere i caratteri corrispondenti utilizzando la funzione "memmove" per spostare gli altri caratteri nella giusta posizione.

```C
#include <stdio.h>
#include <string.h>

void delete_matching_chars(char *str, const char *pattern) {
    char *match = strpbrk(str, pattern); // Trova la prima occorrenza di una delle lettere specificate nel modello
    while (match != NULL) { // Continua finché ci sono corrispondenze
        memmove(match, match+1, strlen(match)); // Rimuove il carattere corrispondente spostando tutti i caratteri successivi a sinistra
        match = strpbrk(match, pattern); // Trova la successiva occorrenza del modello nella nuova stringa
    }
}

int main() {
    char input[] = "1a2b3c4d5e";
    printf("Input: %s\n", input);
    delete_matching_chars(input, "abcde");
    printf("Output: %s\n", input);
    return 0;
}
```
- Output:
```
Input: 1a2b3c4d5e
Output: 12345
```

## Approfondimento

Ci sono diverse varianti di questo approccio, come utilizzare la funzione "strcspn" invece di "strpbrk" per trovare la lunghezza del segmento di stringa da copiare o utilizzare la funzione "strcpy" invece di "memmove". Inoltre, è possibile migliorare le prestazioni minimizzando il numero di chiamate alla funzione "memmove" attraverso la creazione di una nuova stringa di output.

## Vedi anche

- Documentazione della funzione "strpbrk" in C: https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm
- Tutorial sull'utilizzo di stringhe in C: https://www.programiz.com/c-programming/c-strings