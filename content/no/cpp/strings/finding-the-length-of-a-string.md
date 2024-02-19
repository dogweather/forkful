---
aliases:
- /no/cpp/finding-the-length-of-a-string/
date: 2024-01-20 17:47:12.573935-07:00
description: "\xC5 finne lengden av en streng handler om \xE5 telle antall tegn den\
  \ inneholder. Programmerere gj\xF8r dette for \xE5 validere input, formatere tekst,\
  \ eller behandle\u2026"
lastmod: 2024-02-18 23:08:54.175605
model: gpt-4-1106-preview
summary: "\xC5 finne lengden av en streng handler om \xE5 telle antall tegn den inneholder.\
  \ Programmerere gj\xF8r dette for \xE5 validere input, formatere tekst, eller behandle\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## What & Why?
Å finne lengden av en streng handler om å telle antall tegn den inneholder. Programmerere gjør dette for å validere input, formatere tekst, eller behandle data dynamisk.

## How to:
I C++ kan du finne strenglengden ved å bruke `std::string::size()` eller `std::string::length()` funksjonene. 

```C++
#include <iostream>
#include <string>

int main() {
    std::string hilsen = "Hallo, Norge!";
    std::cout << "Lengden er: " << hilsen.size() << std::endl;
    // Alternativt kan du også bruke:
    // std::cout << "Lengden er: " << hilsen.length() << std::endl;
    return 0;
}
```

Output:
```
Lengden er: 13
```

## Deep Dive
Før `std::string` kom i C++98, brukte programmerere C-stil strenger (`char` arrays) og `strlen()` fra `<cstring>` biblioteket for å finne lengden. `std::string::size()` og `std::string::length()` er funksjonelt identiske i moderne C++; bruk den du liker best. Innenfor `std::string` klassen er lengden lagret og oppdatert dynamisk, så funksjonen utføres på O(1) tid.

Alternativer for eldre kode eller spesielle behov inkluderer `strlen()` for C-stil strenger og manuell iterering for å telle tegn. Selv om `std::string`er å foretrekke for enkelhet og sikkerhet, er det viktig å huske på C-stil strenger når man jobber med eksisterende C kodestammer eller systemnivå programmering der standardbiblioteker ikke er tilgjengelige eller ønskede.

## See Also
- C++ Standard Library Reference: https://en.cppreference.com/w/cpp/string/basic_string/size
- C++ Standard Library Reference: https://en.cppreference.com/w/cpp/string/basic_string/length
- C-style strings and `strlen()`: https://en.cppreference.com/w/cpp/string/byte/strlen
