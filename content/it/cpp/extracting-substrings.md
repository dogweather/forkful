---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'estrazione di sottosequenze è l'azione di prendere una parte di una stringa di testo. I programmatori la utilizzano per manipolare, analizzare o semplicemente isolare parti specifiche di una stringa.

## Come fare:
Vediamo un esempio di come estrarre una sottosequenza in C++:

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Ciao, mondo!";
    std::string sub = s.substr(6, 5); // Estrae la sottosequenza

    std::cout << sub << std::endl; // Stampa la sottosequenza
    return 0;
}
```
L'output sarà:

```C++
mondo
```

In questo esempio, abbiamo preso la stringa "Ciao, mondo!", e abbiamo estratto la sottosequenza "mondo" utilizzando il metodo `substr`.

## Un Approfondimento
La funzione `substr` è stata introdotta per la prima volta in C++98 e da allora è un componente fondamentale delle stringhe in C++.

Ci sono alternative all'estrazione di sottosequenze, come l'uso di puntatori o l'iterazione attraverso la stringa, ma `substr` è ampiamente preferita per sua facilità d'uso e chiarezza.

A livello di implementazione, `substr` crea un nuovo oggetto stringa con il contenuto specificato dal indice di inizio e la lunghezza fornita. Quindi, attenzione a non eccedere i limiti della stringa originale, altrimenti solleverà un'eccezione `out_of_range`.

## Vedi Anche
Per ulteriori dettagli sulla manipolazione delle stringhe in C++, consultare: 
- cppreference: [std::string::substr](http://en.cppreference.com/w/cpp/string/basic_string/substr)
- cplusplus.com: [std::basic_string::substr](http://www.cplusplus.com/reference/string/string/substr/)