---
title:    "C: Stampa dell'output di debug"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché
Scopriamo insieme perché è importante stampare l'output di debug nel tuo codice in C. 

Stampare l'output di debug è un'importante pratica di programmazione che aiuta a identificare e risolvere errori nel tuo codice. Quando si sta sviluppando un programma in C, è inevitabile incontrare degli errori lungo il cammino. Invece di dieci puzzolenti cerca di capire dove hai sbagliato, la stampa dell'output di debug può aiutarti a individuare il problema più facilmente e ad affrontarlo in modo efficiente.

## Come fare
Ecco un esempio di codice in C che utilizza la funzione `printf()` per stampare l'output di debug:

```C
#include <stdio.h>

int main()
{
  int num = 42;
  printf("Il valore della variabile num è %d\n", num); //la stringa di format è %d
  return 0;
}
```

In questo codice, stiamo stampando il valore della variabile `num` utilizzando la stringa di formato `%d` all'interno della funzione `printf()`. Ciò significa che il valore della variabile verrà stampato nel punto in cui la stringa di formato è presente nella print statement.

Il risultato dell'esecuzione di questo codice sarà:

```
Il valore della variabile num è 42
```

## Approfondimento
La funzione `printf()` utilizza diverse stringhe di formato per stampare tipi di dati diversi. Ad esempio, `%d` è utilizzato per stampare numeri interi, `%f` per numeri decimali a virgola mobile, `%c` per caratteri e così via. È importante utilizzare la stringa di formato corretta per il tipo di dato che si desidera stampare, altrimenti si otterranno risultati imprevisti.

Inoltre, la stampa dell'output di debug dovrebbe essere usata solo durante lo sviluppo e non dovrebbe essere mantenuta nel codice finale. È consigliabile utilizzare preprocessor macros, come `#ifdef DEBUG`, per controllare se la stampa dell'output di debug è abilitata o meno durante la fase di sviluppo.

## Vedi anche
- [Funzioni di input/output standard in C](https://www.geeksforgeeks.org/input-output-libraries-c/)
- [Printf e Scanf in C: tutorial su come leggere e scrivere su console](https://stackoverflow.com/questions/142508/how-do-i-discard-unwanted-space-characters-during-scanning)
- [Come utilizzare le macro di preprocessor in C](https://www.tutorialspoint.com/cprogramming/c_preprocessors.htm)