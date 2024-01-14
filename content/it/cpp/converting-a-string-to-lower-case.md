---
title:    "C++: Convertire una stringa in caratteri minuscoli"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertingire una stringa in minuscolo può essere utile in molte situazioni, come ad esempio nella validazione di input utente o nella manipolazione di dati. Inoltre, può rendere la gestione di stringhe più efficiente poiché i caratteri minuscoli e maiuscoli vengono considerati diversi nei linguaggi di programmazione come C++.

## Come Fare

Per convertire una stringa in minuscolo in C++, possiamo utilizzare la funzione `tolower()` della libreria standard `cctype`. Questa funzione accetta un carattere come input e restituisce la sua versione in minuscolo.

Di seguito è mostrato un esempio di codice che utilizza la funzione `tolower()` per convertire una stringa in minuscolo e stamparla a schermo:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main() {
  // Definiamo una stringa di esempio
  std::string str = "Ciao a Tutti";

  // Utilizziamo un ciclo for per scorrere tutti i caratteri
  // della stringa
  for (int i = 0; i < str.length(); i++) {

    // Applichiamo la funzione tolower() al carattere corrente
    char c = tolower(str[i]);

    // Sostituiamo il carattere originale con il suo equivalente
    // in minuscolo
    str[i] = c;
  }

  // Stampiamo la stringa in minuscolo
  std::cout << str << std::endl;

  return 0;
}
```

L'output di questo codice sarà:

`ciao a tutti`

## Approfondimento

La funzione `tolower()` della libreria `cctype` è in realtà un wrapper della funzione `tolower()` della libreria `ctype.h`, utilizzata in C. Questa funzione accetta come input un intero rappresentante un carattere e restituisce il suo equivalente in minuscolo.

Quando lavoriamo con stringhe, possiamo utilizzare la funzione `tolower()` carattere per carattere come visto nell'esempio precedente. Oppure, possiamo utilizzare la funzione `std::transform()` della libreria `algorithm`, che accetta una stringa e una funzione (come la funzione `tolower()`) e applica la funzione a ogni carattere della stringa.

Ecco un esempio che utilizza `std::transform()` per convertire una stringa in minuscolo:

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
  // Definiamo una stringa di esempio
  std::string str = "Hello World";

  // Applichiamo la funzione tolower() a ogni carattere
  // della stringa
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);

  // Stampiamo la stringa in minuscolo
  std::cout << str << std::endl;

  return 0;
}
```

L'output di questo codice sarà:

`hello world`

## Vedi Anche

- Documentazione della funzione `tolower()` nella libreria `cctype`: https://www.cplusplus.com/reference/cctype/tolower/
- Documentazione della funzione `transform()` nella libreria `algorithm`: https://www.cplusplus.com/reference/algorithm/transform/
- Altre funzioni utili per la manipolazione di stringhe in C++: https://www.tutorialspoint.com/cplusplus/cpp_strings.htm