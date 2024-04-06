---
date: 2024-01-20 17:41:55.748105-07:00
description: 'How to: ("Come fare:").'
lastmod: '2024-04-05T21:53:44.469310-06:00'
model: gpt-4-1106-preview
summary: ''
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to:
("Come fare:")
```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string text = "Ecco un esempio: 123! Test 456.";
    std::regex pattern("\\d+"); // Rimuove tutte le cifre

    // Usiamo regex_replace per sostituire i numeri con una stringa vuota
    std::string result = std::regex_replace(text, pattern, "");

    std::cout << "Testo originale: " << text << std::endl;
    std::cout << "Dopo rimozione: " << result << std::endl;

    return 0;
}
```
Output:
```
Testo originale: Ecco un esempio: 123! Test 456.
Dopo rimozione: Ecco un esempio: ! Test .
```

## Deep Dive:
("Approfondimento")
C++ ha introdotto la libreria `<regex>` con lo standard C++11, migliorandola nel tempo. Alternativamente puoi rimuovere caratteri iterando il testo, ma `<regex>` rende il codice più leggibile e meno soggetto a errori. A volte `<regex>` può essere più lento di metodi manuali, quindi considera altre soluzioni, come usare la funzione `erase` o `remove_if` dell'STL, se la performance è critica.

## See Also:
("Vedi Anche")
- Documentazione ufficiale della `<regex>` su cppreference.com: [C++ regex](https://en.cppreference.com/w/cpp/regex)
- "Effective Modern C++" di Scott Meyers, per consigli sull'uso efficace delle espressioni regolari in C++ moderno.
- cppreference.com, per approfondire su `std::string::erase`: [string::erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)
