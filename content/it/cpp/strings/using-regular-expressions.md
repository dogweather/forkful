---
date: 2024-01-19
description: "Le espressioni regolari (regexp) servono a trovare pattern di testo\
  \ con regole specifiche. I programmatori le usano per validazione dati, ricerca\
  \ e\u2026"
lastmod: '2024-03-13T22:44:43.716366-06:00'
model: unknown
summary: Le espressioni regolari (regexp) servono a trovare pattern di testo con regole
  specifiche.
title: Utilizzo delle espressioni regolari
weight: 11
---

## Che cosa e perché?
Le espressioni regolari (regexp) servono a trovare pattern di testo con regole specifiche. I programmatori le usano per validazione dati, ricerca e sostituzione in stringhe, facilitando l'automazione di compiti complessi.

## Come fare:
```C++
#include <iostream>
#include <regex>

int main() {
    std::string testo = "Programmare in C++ è divertente!";
    std::regex modello("\\bC\\+\\+\\b");

    // Trova C++ nel testo
    bool contieneCPlusPlus = std::regex_search(testo, modello);
    std::cout << (contieneCPlusPlus ? "Trovato!" : "Non Trovato!") << std::endl;

    // Sostituisci C++ con Rust
    std::string nuovoTesto = std::regex_replace(testo, modello, "Rust");
    std::cout << nuovoTesto << std::endl;

    return 0;
}
```

Output:
```
Trovato!
Programmare in Rust è divertente!
```

## Approfondimento
Le regexp sono nate negli anni '50 con lavori teorici su automi e linguaggi formali. Alternative moderne includono parser dedicati per compiti specifici, ma le regexp rimangono un ottimo strumento per la loro flessibilità ed estesa disponibilità nelle librerie standard. C++ supporta le regex dallo standard C++11 con la libreria `<regex>`.

## Vedi anche
- Documentazione C++ `<regex>`: https://en.cppreference.com/w/cpp/regex
- Tutorial su regexp: https://www.regular-expressions.info/tutorial.html
- Testare regexp online: https://regexr.com/
