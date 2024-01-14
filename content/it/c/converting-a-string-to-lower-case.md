---
title:    "C: Convertire una stringa in minuscolo"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è una funzionalità comune nei linguaggi di programmazione, ed è utile in molte situazioni. Ad esempio, potresti voler confrontare due stringhe senza distinguere tra lettere maiuscole e minuscole, oppure dover rendere uniforme il formato dei dati inseriti dall'utente.

## Come fare

La conversione di una stringa in minuscolo è una semplice operazione che può essere realizzata con poche linee di codice in linguaggio C. Per prima cosa, è necessario includere la libreria "string.h" nel tuo programma. In seguito, è possibile utilizzare la funzione "tolower()" per convertire ogni carattere della stringa in minuscolo, come mostrato nel seguente esempio:

```C
#include <stdio.h>
#include <string.h>

int main() {

    char stringa[] = "Hello World";
    int lunghezza = strlen(stringa); // otteniamo la lunghezza della stringa
    int i;

    for (i = 0; i < lunghezza; i++) {
        stringa[i] = tolower(stringa[i]); // utilizziamo tolower() per convertire il carattere in minuscolo
    }

    printf("%s\n", stringa); // stampiamo la stringa convertita in minuscolo
    
    return 0;
}
```

Questo codice produrrà l'output "hello world", come desiderato.

## Approfondimento

La funzione "tolower()" è definita all'interno della libreria "ctype.h" ed è utilizzata per convertire un carattere in minuscolo. Tuttavia, è importante notare che questa funzione funziona solo con i caratteri ASCII e non funziona con caratteri di altri set di caratteri.

Un'altra cosa da tenere a mente è che la funzione "tolower()" restituirà sempre il valore ASCII di un carattere, anche se lo passiamo un valore diverso da un carattere. In questo caso, poiché il risultato è limitato all'intervallo di valori ASCII, è possibile che vengano restituiti risultati imprevisti o inaspettati.

Inoltre, alcuni caratteri potrebbero non essere convertiti in minuscolo come ci si aspetta. Questo dipende dalle impostazioni e dal set di caratteri del sistema in cui viene eseguito il programma.

## Vedi anche

- [Converting Strings to Lowercase in C](https://www.programiz.com/c-programming/library-function/ctype/tolower)
- [Standard Library Reference for toupper and tolower functions in C](https://www.cs.uic.edu/~jbell/CourseNotes/C_Programming/SourceFiles/c001.htm)
- [Understanding ASCII and Unicode Characters](https://www.computerhope.com/jargon/a/ascii.htm)