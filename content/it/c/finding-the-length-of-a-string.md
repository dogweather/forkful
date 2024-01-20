---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# L'arte di trovare la lunghezza di una stringa in C

## Cos'è e Perché?
Trovare la lunghezza di una stringa significa contare il numero di caratteri che compongono una stringa. È una pratica comune tra i programmatori poiché ci aiuta a manipolare o processare i dati testuali in modi più precisi ed efficienti.

## Come fare:
Ecco un esempio di come si trova la lunghezza di una stringa in C utilizzando la funzione ```strlen```.

```C
#include<stdio.h>
#include<string.h>

int main() {
    char s[] = "Programmazione in C";
    int length = strlen(s);

    printf("La lunghezza della stringa è: %d", length);
    return 0;
}
```

In questo codice, ```strlen``` calcola il numero di caratteri nella stringa "Programmazione in C", escludendo il carattere nullo alla fine. L'output sarà "La lunghezza della stringa è: 20".

## Approfondimento
Como partiamo dalle fondamenta: la funzione ```strlen``` è stata introdotta nel C Standard Library sin dal primo standard C che è stato pubblicato nel 1989 (C89).

Esistono alternative a ```strlen```. Puoi, ad esempio, creare una funzione personalizzata che itera attraverso la stringa, contando i caratteri fino a raggiungere il carattere nullo.

```C
int stringLength(char *s) {
    int i = 0;
    while(s[i] != '\0') {
        i++;
    }
    return i;
}
```

Note that ```strlen``` is actually implemented in a similar—though more complex—way to ensure efficiency.

## Vedere Anche
[Riferimento alla libreria standard C - strlen](http://www.cplusplus.com/reference/cstring/strlen/)
[Tutorial su come utilizzare le stringhe in C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)