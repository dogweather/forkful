---
date: 2024-01-20 17:34:34.955581-07:00
description: Concatenare le stringhe significa unirle in una sola. Lo facciamo per
  creare frasi o valori che necessitano di parti dinamiche e statiche insieme.
lastmod: '2024-03-13T22:44:43.718336-06:00'
model: gpt-4-1106-preview
summary: Concatenare le stringhe significa unirle in una sola.
title: Concatenazione di stringhe
weight: 3
---

## What & Why? (Cosa e Perché?)
Concatenare le stringhe significa unirle in una sola. Lo facciamo per creare frasi o valori che necessitano di parti dinamiche e statiche insieme.

## How to (Come fare)
In C++ si possono concatenare stringhe con l'operatore `+` o con il metodo `append`. Ecco come:

```C++
#include <iostream>
#include <string>

int main() {
    std::string saluto = "Ciao, ";
    std::string nome = "Giulia";
    std::string messaggio = saluto + nome + "!";

    std::cout << messaggio << std::endl; // Output: Ciao, Giulia!

    // Usando append()
    std::string str1 = "Buongiorno, ";
    std::string str2 = "Marco";
    str1.append(str2); // str1 ora è "Buongiorno, Marco"

    std::cout << str1 << std::endl; // Output: Buongiorno, Marco

    return 0;
}
```

## Deep Dive (Approfondimento)
Concatenare stringhe è essenziale per creare output dinamici. Nato nel C con l'uso di `strcat` e array di char, il C++ ha portato questo concetto al livello successivo con la classe `std::string` che ha semplificato il processo.

Prima di `std::string`, la concatenazione in C richiedeva una gestione manuale dei buffer e attenzione a non superare la memoria allocata. In C++ tutto ciò è gestito automaticamente.

Ci sono alternative come `stringstream` o `fmt` (della libreria {fmt}), che consentono una formattazione più complessa e concatenazioni avanzate:

```C++
#include <iostream>
#include <sstream>
#include <fmt/format.h>

int main() {
    // Usando stringstream
    std::stringstream ss;
    ss << "Hello, " << "World!";
    std::string s = ss.str();
    std::cout << s << std::endl; // Output: Hello, World!

    // Usando {fmt}
    std::string fs = fmt::format("{}{}", "Ciao ", "Mondo!");
    std::cout << fs << std::endl; // Output: Ciao Mondo!

    return 0;
}
```
 
Ogni metodo ha i suoi pro e contro:

- `+` e `append()` sono semplici ma possono essere meno efficienti in cicli.
- `stringstream` è potente per combinare testo e numeri ma leggermente più verboso.
- `{fmt}` è moderno e veloce, con una sintassi chiara, parte dallo standard C++20.

## See Also (Vedi Anche)
- C++ Reference for `std::string`: https://cplusplus.com/reference/string/string/
- C++ Reference for `stringstream`: https://cplusplus.com/reference/sstream/
- {fmt} Library Documentation: https://fmt.dev/latest/index.html
