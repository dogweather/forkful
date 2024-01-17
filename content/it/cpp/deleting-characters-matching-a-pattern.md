---
title:                "Eliminazione dei caratteri corrispondenti a un determinato modello"
html_title:           "C++: Eliminazione dei caratteri corrispondenti a un determinato modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un determinato modello"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Cancellazione di caratteri corrispondenti a un pattern in C++: Una guida rapida

## Che cosa & Perché?
La cancellazione dei caratteri corrispondenti a un pattern è un processo comune per i programmatori, che consiste nell'eliminare dal testo tutti i caratteri che soddisfano un determinato criterio. Ciò può essere utile per ripulire il testo da dati indesiderati o per sostituire parti di testo con stringhe vuote.

## Come fare:
Di seguito sono riportati alcuni esempi di codice in C++ per eliminare i caratteri corrispondenti a un pattern e l'output corrispondente.

```C++
// Elimina tutte le vocali da una stringa
#include <iostream>
#include <string>
#include <algorithm>

int main()
{
    std::string s = "Ciao, come stai?";
    s.erase(std::remove_if(s.begin(), s.end(), [](char c) { return std::tolower(c) == 'a' || std::tolower(c) == 'e' || std::tolower(c) == 'i' || std::tolower(c) == 'o' || std::tolower(c) == 'u'; }), s.end());
    
    std::cout << s << std::endl;
    // Output: C, cm st?
    
    return 0;
}
```

```C++
// Elimina tutti i numeri da una stringa
#include <iostream>
#include <string>
#include <algorithm>

int main()
{
    std::string s = "Questa stringa contiene 123 numeri!";
    s.erase(std::remove_if(s.begin(), s.end(), [](char c) { return std::isdigit(c); }), s.end());
    
    std::cout << s << std::endl;
    // Output: Questa stringa contiene numeri!
    
    return 0;
}
```

## Approfondimento:
La tecnica di cancellazione dei caratteri corrispondenti a un pattern è stata introdotta con l'introduzione del linguaggio C nel 1972 da Ken Thompson. Esistono anche altre tecniche per la rimozione di caratteri da una stringa, come l'utilizzo delle espressioni regolari o dei loop, tuttavia la cancellazione di caratteri corrispondenti a un pattern è spesso la soluzione più efficiente e veloce.

## Vedi anche:
- [Documentazione di C++](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [Tutorial su espressioni regolari in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-with-examples/)
- [Discussione su Stack Overflow riguardo la cancellazione dei caratteri in una stringa](https://stackoverflow.com/questions/16329358/c-efficient-way-to-remove-all-occurrences-of-a-character)