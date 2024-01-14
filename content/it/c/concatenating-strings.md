---
title:    "C: Concatenazione di stringhe"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
La concatenazione di stringhe è una tecnica comune utilizzata nel linguaggio di programmazione C per combinare più stringhe in un'unica stringa. Questa operazione è utile per creare output ben formattati e può essere utilizzata in una varietà di applicazioni.

## Come fare
Il concatentamento di stringhe in C è semplice e può essere fatto utilizzando la funzione `strcat()`. Di seguito è riportato un esempio di codice che concatena due stringhe e ne stampa il risultato:

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Ciao ";
    char str2[20] = "amici";
    strcat(str1, str2);
    printf("Il risultato è: %s", str1);
    return 0;
}
```

```
Output:
Il risultato è: Ciao amici
```

La funzione `strcat()` prende due stringhe come argomento e concatena la seconda stringa alla prima. La prima stringa deve avere abbastanza spazio per contenere la seconda stringa concatenata. È importante notare che la funzione `strcat()` modifica la prima stringa passata come argomento, quindi è necessario assicurarsi che sia sufficientemente grande prima di utilizzarla.

## Approfondimento
Nella programmazione C, le stringhe sono considerate come array di caratteri e quindi possono essere trattate come tali. Questo significa che possiamo concatenare due stringhe manualmente utilizzando un ciclo `for` per scorrere le stringhe e copiarne i caratteri in una terza stringa.

Di seguito è riportato un esempio di codice che utilizza questa tecnica:

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Ciao ";
    char str2[20] = "amici";
    char str3[100];
    int i, j;

    for(i=0; str1[i]!='\0'; i++){
        str3[i] = str1[i];
    }

    for(j=0; str2[j]!='\0'; j++){
        str3[i] = str2[j];
        i++;
    }

    str3[i] = '\0';
    printf("Il risultato è: %s", str3);
    return 0;
}
```

```
Output:
Il risultato è: Ciao amici
```

In questo esempio, la terza stringa `str3` viene utilizzata per contenere la stringa concatenata. Il ciclo `for` scorre la prima stringa `str1` e copia i suoi caratteri in `str3`. Poi, utilizzando un secondo ciclo `for`, scorre la seconda stringa `str2` e li aggiunge alla fine di `str3`. Infine, viene aggiunto il carattere terminatore `'\0'` per indicare la fine della stringa.

## Vedi anche
- [Tutorial su stringhe in C](https://www.programiz.com/c-programming/c-strings)
- [Documentazione ufficiale di `strcat()`](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Esempi di concatenamento di stringhe in C](https://www.geeksforgeeks.org/concatenate-strings-in-c-4-different-ways/)