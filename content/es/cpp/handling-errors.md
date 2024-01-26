---
title:                "Manejo de errores"
date:                  2024-01-26T00:49:52.602267-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Manejar errores significa planificar para cuando las cosas salen mal. Es vital porque ayuda a evitar fallos y hace que tu software sea robusto y amigable para el usuario.

## Cómo hacerlo:
Aquí tienes un bloque básico try-catch para manejar una excepción:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("¡Ups! Algo salió mal.");
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
    return 0;
}
```

Salida de muestra:
```
Error: ¡Ups! Algo salió mal.
```

## Inmersión Profunda
C++ ha tenido manejo de errores desde sus primeros días. La forma más básica era comprobar valores de retorno. Si has estado en esto por un tiempo, recordarás los días pre-estándar: C con clases y comprobación de errores manual.

Luego vinieron las excepciones con C++ para darnos una forma estructurada de lidiar con problemas inesperados. Una excepción se lanza con `throw` y se captura con `try/catch`.

Dos tipos de errores suelen surgir: errores lógicos, como un cálculo incorrecto, y errores de ejecución, como acceder a una dirección de memoria no válida. Las excepciones son ideales para errores de ejecución. Para errores lógicos, a menudo es mejor usar afirmaciones o códigos de error.

Hay un debate en curso sobre excepciones vs. códigos de error. Las excepciones pueden ser más lentas y pueden llevar a flujos de control complejos. Los códigos de error, aunque más rápidos, pueden hacer que el código esté abarrotado y sea más difícil de mantener. Es una compensación, por lo que conocer tu caso de uso es clave.

C++17 introdujo `std::optional` y `std::variant`, que son alternativas a las excepciones. Son útiles para funciones que pueden o no devolver un resultado válido.

La seguridad de las excepciones puede ser otro dolor de cabeza. Se trata de garantías que tu código proporciona a pesar de las excepciones. Hay tres niveles: básico, fuerte y nothrow. Cuantas más garantías, más complejo podría ser tu código.

Pensamientos finales: el manejo de errores es tanto arte como ciencia. Da forma a cómo tu aplicación sobrevive en el entorno real. No abuses de las excepciones. Apunta a un código legible y mantenible.

## Ver también
- [cppreference sobre manejo de excepciones](https://en.cppreference.com/w/cpp/language/exceptions)
- [La perspectiva de Bjarne Stroustrup sobre el manejo de errores](http://www.stroustrup.com/except.pdf)
- [Directrices del núcleo de C++ sobre excepciones](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)