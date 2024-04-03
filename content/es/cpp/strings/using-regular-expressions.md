---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:03.680713-07:00
description: "C\xF3mo hacerlo: C++11 introdujo soporte para expresiones regulares\
  \ en la biblioteca est\xE1ndar, `<regex>`, ofreciendo un marco robusto para la b\xFA\
  squeda y\u2026"
lastmod: '2024-03-13T22:44:59.365572-06:00'
model: gpt-4-0125-preview
summary: "C++11 introdujo soporte para expresiones regulares en la biblioteca est\xE1\
  ndar, `<regex>`, ofreciendo un marco robusto para la b\xFAsqueda y manipulaci\xF3\
  n de cadenas."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
C++11 introdujo soporte para expresiones regulares en la biblioteca estándar, `<regex>`, ofreciendo un marco robusto para la búsqueda y manipulación de cadenas. Aquí hay un ejemplo básico de cómo usar expresiones regulares para buscar un patrón dentro de una cadena:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hola, mi correo es ejemplo@ejemplo.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "¡Correo encontrado!" << std::endl;
    } else {
        std::cout << "No se encontró correo." << std::endl;
    }

    return 0;
}
```
**Salida de muestra**
```
¡Correo encontrado!
```

Para manipulaciones más complejas, como reemplazar patrones dentro de cadenas, las expresiones regulares de C++ pueden ser muy útiles:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "La lluvia en España cae principalmente en la llanura.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**Salida de muestra**
```
L* ll*v** *n Esp**ñ* c** pr*nc*p*lm*nt* *n l* ll*n*r*.
```

Para los programadores que exploran más allá de la biblioteca estándar, la Biblioteca Regex de Boost (`boost/regex.hpp`) es una opción de terceros popular que ofrece capacidades de regex mejoradas y optimizaciones de rendimiento, particularmente para patrones complejos o procesamiento de datos extensivos:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "¡Las bibliotecas Boost son divertidas!";
    boost::regex expr("(\\w+)\\s(bibliotecas)"); // Coincidir con "Bibliotecas Boost"
    std::string fmt("GNU \\1"); // Reemplazar con "GNU Boost"

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**Salida de muestra**
```
GNU Boost son divertidas!
```

Estos ejemplos apenas rasguñan la superficie de las capacidades de C++ con expresiones regulares, ilustrando búsquedas básicas, emparejamiento de patrones y reemplazos, ya sea usando la biblioteca estándar o potenciado por la poderosa implementación de regex de Boost.
