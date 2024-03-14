---
date: 2024-01-20 17:50:49.396969-07:00
description: "Strenginterpolasjon betyr \xE5 stikke variabler eller uttrykk inn i\
  \ en streng. Det gj\xF8r koden mer lesbar og lar oss lett bygge dynamiske meldinger."
lastmod: '2024-03-13T22:44:41.085625-06:00'
model: gpt-4-1106-preview
summary: "Strenginterpolasjon betyr \xE5 stikke variabler eller uttrykk inn i en streng.\
  \ Det gj\xF8r koden mer lesbar og lar oss lett bygge dynamiske meldinger."
title: Interpolering av en streng
---

{{< edit_this_page >}}

## What & Why?
Strenginterpolasjon betyr å stikke variabler eller uttrykk inn i en streng. Det gjør koden mer lesbar og lar oss lett bygge dynamiske meldinger.

## How to:
I C++20 har vi `std::format`, som lar oss formatere og interpolere strenger på en lignende måte som i Python. 

```C++
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string name = "Ola";
    std::string greeting = std::format("Hei, {}! Du er {} år gammel.", name, age);
    std::cout << greeting << std::endl;
    return 0;
}
```

Output:
```
Hei, Ola! Du er 30 år gammel.
```

Før C++20 var vi ofte avhengig av `stringstream` eller kople strenger og variabler sammen med `+`.

```C++
#include <sstream>
#include <iostream>
#include <string>

int main() {
    int age = 30;
    std::string name = "Ola";
    
    std::ostringstream oss;
    oss << "Hei, " << name << "! Du er " << age << " år gammel.";
    std::string greeting = oss.str();
    
    std::cout << greeting << std::endl;
    return 0;
}
```

Output:
```
Hei, Ola! Du er 30 år gammel.
```

## Deep Dive
Før `std::format` i C++20, var strenginterpolasjon mer tungvint. Vi brukte `std::ostringstream` eller gamle C-stil `sprintf`, som var mindre sikker og ga rom for feil. `std::format` er inspirert av Python's f-strings og `.format`, og det gir en type-sikker og leser-vennlig måte å formatere strenger på.

Alternativer eksisterer, inkludert `boost::format` for eldre prosjekter, eller bruk av biblioteker som `fmt`, som `std::format` faktisk er basert på.

Implementasjonsdetaljer: `std::format` håndterer forskjellige datatyper og kompliserte objekter ved å kreve at de har en tilpasset formatter funksjon om nødvendig. Det bruker også "{}" for plassholdere og lar deg spesifisere detaljer som bredde, padding, og presisjon.

## See Also
- [cppreference.com/std/format](https://en.cppreference.com/w/cpp/utility/format) - Offisiell dokumentasjon for `std::format`.
- [fmtlib.net](https://fmt.dev/latest/index.html) - `fmt` bibliotekets hjemmeside, som lå som grunnlag for `std::format`.
