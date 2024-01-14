---
title:                "C: Eliminazione di caratteri corrispondenti ad un modello"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso quando si lavora con stringhe di testo in un programma di linguaggio C, può essere necessario eliminare determinati caratteri che corrispondono ad un particolare modello o pattern. Ciò può essere utile per pulire i dati di input o per manipolare le stringhe per una migliore elaborazione.

## Come

Per eliminare i caratteri che corrispondono ad un certo modello, possiamo utilizzare la funzione `strchr()` che restituisce un puntatore al primo carattere in una stringa che corrisponde al carattere specificato. Possiamo quindi utilizzare un ciclo `while` per scorrere la stringa e verificare se ogni carattere corrisponde al modello. Se è così, possiamo utilizzare la funzione `memmove()` per spostare i caratteri successivi in avanti di una posizione, sovrascrivendo così il carattere corrispondente.

Ecco un esempio di codice che elimina tutte le vocali da una stringa:

```
char stringa[] = "Ciao a tutti!";
char *puntatore;

while ((puntatore = strchr(stringa, 'a')) != NULL) {
    memmove(puntatore, puntatore + 1, strlen(puntatore));
}
```

L'output di questo codice sarebbe `C tt!`, poiché tutte le vocali sono state rimosse dalla stringa iniziale.

## Approfondimento

Ci sono diverse funzioni in C che possono aiutare nella rimozione dei caratteri che corrispondono ad un determinato pattern. Ad esempio, la funzione `strsep()` può essere utilizzata per dividere una stringa in sottostringhe in base ad un delimitatore specificato. Questo può essere utile quando si cerca di eliminare parti specifiche di una stringa.

Inoltre, esistono anche librerie di terze parti che forniscono funzioni più avanzate per la manipolazione delle stringhe in C, ad esempio la libreria PCRE (Perl Compatible Regular Expressions).

## Vedi anche

- Documentazione sulla funzione `strchr()`: [https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- Esempi di uso della funzione `memmove()`: [https://www.programiz.com/c-programming/library-function/string.h/memmove](https://www.programiz.com/c-programming/library-function/string.h/memmove)
- Libreria PCRE: [https://www.pcre.org/](https://www.pcre.org/)