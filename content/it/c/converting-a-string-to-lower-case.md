---
title:    "C: Convertire una stringa in minuscolo"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Convertire una stringa in minuscolo può essere utile per confrontare due stringhe senza prendere in considerazione le maiuscole e per rendere più facile la ricerca di un determinato testo all'interno di una stringa.

## Come Fare
Per convertire una stringa in minuscolo in linguaggio C, è possibile utilizzare la funzione `tolower()` della libreria `ctype.h`. Questa funzione prende come parametro un carattere e lo converte in minuscolo. Di seguito un esempio di codice:

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
  char str[] = "CIAO AMICI";
  int i = 0;
  
  while (str[i]) {
    str[i] = tolower(str[i]);
    i++;
  }

  printf("%s", str);
  // Output: ciao amici
  return 0;
}
```

## Approfondimento
In questo esempio, abbiamo utilizzato un loop `while` per scorrere ogni carattere della stringa e applicare la funzione `tolower()` su di esso. Possiamo notare che la funzione modifica direttamente la stringa originale e non è necessario assegnare il nuovo valore ad una variabile.

È importante notare che la funzione `tolower()` può essere utilizzata solo su un carattere alla volta e non su un'intera stringa. Inoltre, questa funzione non effettua verifica sull'input, quindi è importante assicurarsi che il parametro passato sia un carattere valido.

## Vedi Anche
- [Funzione tolower() - Documentazione di Microsoft](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/tolower?view=msvc-160)
- [Libreria ctype.h - Documentazione di cplusplus.com](http://www.cplusplus.com/reference/cctype/)