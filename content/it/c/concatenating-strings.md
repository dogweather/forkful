---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenazione di Stringhe in C

## Cosa & Perché?
La concatenazione di stringhe è l'operazione di unire due o più stringhe in una singola stringa. I programmatori fanno ciò per manipolare e trasformare i dati testuali.

## Come fare:
In C, la funzione `strcat()` del library `string.h` si usa per concatenare stringhe.
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Ciao, ";
    char str2[] = "come stai?";
    
    strcat(str1, str2);
    printf("%s\n", str1);
    
    return 0;
}
```
Output:
```C
Ciao, come stai?
```

## Approfondimenti
La funzione `strcat()` è presente in C sin dalla sua prima versione (1972). Alternativamente, puoi usare `strncat()` se vuoi concatenare un numero specifico di caratteri. Attenzione: `strcat()` non controlla l'overflow del buffer. Assicurati di avere abbastanza spazio nella stringa di destinazione.

## Vedi Anche 
- [C Library - <string.h>](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Secure Coding in C and C++: Strings](https://resources.sei.cmu.edu/asset_files/Presentation/2013_017_001_497063.pdf)