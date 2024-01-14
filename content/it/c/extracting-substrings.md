---
title:    "C: Estrazione di sottostringhe"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Quando si lavora con stringhe in linguaggio C, una delle operazioni comuni è l'estrazione di sottostringhe. Questo può essere utile nel caso in cui si voglia manipolare parti specifiche di una stringa o confrontare stringhe tra loro. In questo articolo vedremo come effettuare l'estrazione di sottostringhe in C.

## Come Fare
L'estrazione di sottostringhe in C è semplice e diretta. Basta seguire questi passaggi:
1. Definire una stringa da cui estrarre la sottostringa.
2. Definire gli indici di inizio e fine della sottostringa.
3. Utilizzare la funzione `strncpy()` per copiare la sottostringa nella variabile desiderata.

Vediamo un esempio di codice che estrae una sottostringa dalla stringa `"Hello, World!"`:
```C
#include <stdio.h>
#include <string.h>

int main()
{
    char string[20] = "Hello, World!";
    char sub_string[10];
    
    // Estraiamo la sottostringa "World" dalla stringa
    strncpy(sub_string, string + 7, 5);
    
    // Stampiamo la sottostringa
    printf("La sottostringa è: %s\n", sub_string);
    
    return 0;
}
```
L'output di questo codice sarà:
```
La sottostringa è: World
```
Come si può vedere, la funzione `strncpy()` copia esattamente la lunghezza della sottostringa specificata, quindi è importante specificare uno spazio sufficiente nella variabile di destinazione.

## Approfondimento
Nel linguaggio C, una stringa è semplicemente un array di caratteri terminato da `\0`, quindi per estrarre una sottostringa basta manipolare gli indici dell'array di caratteri. Inoltre, è possibile utilizzare la funzione `strstr()` per trovare una sottostringa specifica all'interno di una stringa.

## Vedi Anche
- [Funzione strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Funzione strstr()](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Tutorial su stringhe in C](https://www.learn-c.org/en/Strings)