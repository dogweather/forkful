---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares (regex) permiten buscar patrones en textos. Los programadores las usan para validar, encontrar o reemplazar datos de manera eficiente y flexible.

## Cómo hacerlo:

En C++ con la librería estándar `<regex>`, puedes usar expresiones regulares así:

```cpp
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string texto = "Encuentrame en info@example.com o en web@example.net";
    std::regex patron_email(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b)");

    std::smatch resultados;
    
    while (std::regex_search(texto, resultados, patron_email)) {
        std::cout << "Email encontrado: " << resultados[0] << '\n';
        texto = resultados.suffix().str();
    }
    
    return 0;
}
```

Salida:
```
Email encontrado: info@example.com
Email encontrado: web@example.net
```

## Profundización

Las regex existen desde los años 50, evolucionando desde teoría de autómatas hasta herramientas prácticas en la mayoría de lenguajes de programación. Alternativas a regex incluyen el procesamiento de texto vía análisis sintáctico (parsing) cuando se necesitan estructuras más complejas. En C++, regex se implementa mediante la clase std::regex y funciones asociadas en la cabecera `<regex>`.

## Ver También

- Documentación de regex en cppreference: [https://en.cppreference.com/w/cpp/regex](https://en.cppreference.com/w/cpp/regex)
- "Mastering Regular Expressions" por Jeffrey E.F. Friedl, un recurso completo para entender y aplicar regex.
- Herramienta online para probar expresiones regulares: [https://regex101.com/](https://regex101.com/)
