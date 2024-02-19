---
aliases:
- /es/cpp/interpolating-a-string/
date: 2024-01-20 17:50:33.681689-07:00
description: "La interpolaci\xF3n de cadenas permite insertar valores de variables\
  \ dentro de una cadena de texto. Los programadores lo hacen para construir mensajes\u2026"
lastmod: 2024-02-18 23:09:10.293263
model: gpt-4-1106-preview
summary: "La interpolaci\xF3n de cadenas permite insertar valores de variables dentro\
  \ de una cadena de texto. Los programadores lo hacen para construir mensajes\u2026"
title: "Interpolaci\xF3n de cadenas de texto"
---

{{< edit_this_page >}}

## Qué y por qué?
La interpolación de cadenas permite insertar valores de variables dentro de una cadena de texto. Los programadores lo hacen para construir mensajes dinámicamente, facilitar la localización y mejorar la legibilidad del código.

## Cómo hacerlo:
Con la versión más reciente de C++, puedes utilizar `std::format` para la interpolación de cadenas de forma sencilla:

```C++
#include <iostream>
#include <format>

int main() {
    std::string nombre = "Mundo";
    int visitantes = 42;

    // Interpolación de cadena usando std::format
    std::string saludo = std::format("¡Hola, {}! Has tenido {} visitantes.", nombre, visitantes);
    std::cout << saludo << std::endl;

    return 0;
}
```

Salida:
```
¡Hola, Mundo! Has tenido 42 visitantes.
```

## Deep Dive
Antes de C++20, los programadores utilizaban flujos de salida o funciones como `sprintf` para formatear texto. Con `std::format`, introducido en C++20, se agregó una forma más segura y conveniente de hacerlo al estilo de Python. 

Alternativas a `std::format` son la concatenación manual y el uso de librerías como Boost.Format antes de C++20. El uso de `std::format` es preferible por su tipo seguro y rendimiento comparable.

Detalles de implementación: `std::format` usa internamente un sistema de análisis de formato de texto para reemplazar las llaves `{}` con el valor de las variables proporcionadas, manejando tipos y conversiones automáticamente.

## Ver también

- Documentación de `std::format` en cppreference: [cppreference.com/w/cpp/utility/format](https://en.cppreference.com/w/cpp/utility/format)
- Historia de la formateo de cadenas en C++: [cppreference.com/w/cpp/io/c/fprintf](https://en.cppreference.com/w/cpp/io/c/fprintf)
- Alternativas de Boost.Format: [boost.org/doc/libs/release/libs/format/](https://www.boost.org/doc/libs/release/libs/format/)
