---
title:    "C: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti dover eliminare caratteri che corrispondono a un determinato pattern dal tuo codice C. Potrebbe essere necessario per eseguire una pulizia dei dati, filtrare informazioni non necessarie o semplicemente per eseguire una modifica specifica del tuo testo. In ogni caso, la capacità di eliminare caratteri in modo efficiente è un'abilità importante da avere nella programmazione in linguaggio C.

## Come fare

Per eliminare caratteri che corrispondono a un pattern in C, puoi utilizzare la funzione `strpbrk ()`. Questa funzione cerca una stringa di caratteri specificata in un'altra stringa e restituisce un puntatore al primo carattere corrispondente trovato. Usando questa funzione, puoi scrivere una semplice funzione che elimina tutti i caratteri corrispondenti da una stringa fornita. Ecco un esempio di codice:

```C
#include <stdio.h>
#include <string.h>

// Funzione per eliminare un determinato carattere da una stringa
void delete_char(char* str, char c) {
    char* ptr = strpbrk(str, &c); // Trova il primo carattere corrispondente
    while (ptr != NULL) {
        memmove(ptr, ptr + 1, strlen(ptr)); // Sposta il resto della stringa di un carattere a sinistra
        ptr = strpbrk(str, &c); // Trova il prossimo carattere corrispondente
    }
}

int main() {
    char str[] = "Hello World";
    printf("Stringa originale: %s \n", str);
    delete_char(str, 'l'); // Elimina tutti i caratteri "l" dalla stringa
    printf("Stringa modificata: %s \n", str);
    return 0;
}
```

Esempio di output:

```
Stringa originale: Hello World
Stringa modificata: Heo Word 
```

Nell'esempio sopra, la funzione `delete_char` prende come argomenti una stringa e un carattere da eliminare. Viene utilizzata la funzione `strpbrk ()` per trovare il primo carattere corrispondente nella stringa e successivamente vengono eliminati tutti i caratteri corrispondenti spostando il resto della stringa di un carattere a sinistra.

Oltre alla funzione `strpbrk ()`, è anche possibile utilizzare altre funzioni come `strchr ()` e `strstr ()` per eliminare caratteri corrispondenti da una stringa.

## Approfondimento

Eliminare caratteri che corrispondono a un pattern può essere un'operazione utile quando si lavora con input utente o con file di dati non strutturati. Tuttavia, è importante prestare attenzione alla gestione della memoria quando si manipolano stringhe in questo modo. Assicurarsi di allocare memoria sufficiente per la stringa modificata e di liberare eventuali spazi di memoria non utilizzati per evitare memory leaks.

## Vedi anche

- [Tutorial: Stringhe in C](https://www.programiz.com/c-programming/c-strings)
- [Funzione strpbrk() in C](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [Gestione della memoria in C](https://www.tutorialspoint.com/cprogramming/c_memory_management.htm)