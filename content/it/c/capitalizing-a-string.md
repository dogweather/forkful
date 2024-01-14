---
title:    "C: Capitalizzazione di una stringa"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Se ti sei mai chiesto come capitalizzare una stringa in C, sei nel posto giusto. Questa breve guida ti spiegherà come farlo in modo semplice e veloce.

## Come Fare

Per capitalizzare una stringa, devi seguire questi semplici passaggi:

1. Dichiarare una stringa di caratteri (char array) che vogliamo capitalizzare.
2. Scorrere ogni carattere della stringa e utilizzare la funzione `toupper()` per convertirlo in maiuscolo.
3. Stampa la nuova stringa capitalizzata.

Vediamo come si fa nel codice qui sotto:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char string[] = "ciao a tutti";
    int len = sizeof(string) / sizeof(char); // Lunghezza della stringa
    int i;

    // Ciclo per scorrere ogni carattere
    for(i = 0; i < len; i++) {
        string[i] = toupper(string[i]); // Trasformiamo in maiuscolo
    }

    printf("Stringa capitalizzata: %s\n", string); // Stampa la nuova stringa
    return 0;
}
```

Se eseguiamo questo codice, otterremo l'output:

`Stringa capitalizzata: CIAO A TUTTI`

## Deep Dive

La funzione `toupper()` è definita nella libreria `ctype.h` e viene utilizzata per convertire un singolo carattere in maiuscolo. La sua dichiarazione è `int toupper(int c)`. Questa funzione restituisce il carattere trasformato in maiuscolo, quindi è importante assegnare il risultato alla variabile che contiene il carattere per ottenere il risultato desiderato.

Inoltre, è possibile utilizzare un semplice algoritmo per capitalizzare una stringa senza dover utilizzare la funzione `toupper()`. Ci sono molte varianti di questo algoritmo, ma la maggior parte di esse è simile al seguente:

1. Puoi utilizzare un ciclo per scorrere ogni carattere della stringa.
2. Se un carattere è compreso tra 'a' e 'z', puoi sottrarre 32 dal suo valore ASCII per convertirlo in maiuscolo.
3. Puoi ripetere questo processo per ogni carattere della stringa.

## Vedi Anche

- [Funzione `toupper()`](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Esempi di utilizzo della funzione `toupper()`](https://www.geeksforgeeks.org/toupper-function-in-c/)
- [Algoritmi per capitalizzare una stringa in C](https://www.sanfoundry.com/c-program-to-capitalizes-the-given-string/)

__END__