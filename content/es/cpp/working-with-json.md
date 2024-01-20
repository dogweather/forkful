---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por qué?
JSON es el formato estándar para intercambiar datos en aplicaciones web. Los programadores lo usan por ser ligero, fácil de leer para humanos y fácil de analizar para máquinas.

## Cómo hacerlo:
Para trabajar con JSON en C++, puedes usar la biblioteca `nlohmann/json`. Primero, instálala e inclúyela.

```cpp
#include <nlohmann/json.hpp>
// para conveniencia
using json = nlohmann::json;

int main() {
    // Crear un objeto JSON a partir de una cadena
    json j = json::parse(R"({"nombre":"Juan", "edad":30, "programador":true})");
    
    // Acceder a los valores
    std::string nombre = j["nombre"];
    int edad = j["edad"];
    
    // Imprimir los resultados
    std::cout << "Nombre: " << nombre << "\nEdad: " << edad << std::endl;
    
    return 0;
}
```

Salida:
```
Nombre: Juan
Edad: 30
```

## Por Dentro:
JSON nació en los 2000, diseñado por Douglas Crockford, inspirándose en la sintaxis de objetos de JavaScript. Alternativas incluyen XML y YAML, pero JSON prevalece en REST APIs por ser más compacto. Al manejarlo en C++, considera:

- Performance: `nlohmann/json` es amigable pero, para máxima eficiencia, mira `simdjson`.
- Interoperabilidad: Asegúrate de que el JSON que generes cumple con el estándar (RFC 7159).

## Ver También:
- [json.org](https://www.json.org/json-es.html): Documentación oficial de JSON.
- [GitHub de nlohmann/json](https://github.com/nlohmann/json): Documentación y ejemplos de `nlohmann/json`.
- [RFC 7159](https://tools.ietf.org/html/rfc7159): El estándar de JSON.
- [simdjson GitHub](https://github.com/simdjson/simdjson): Una biblioteca C++ de alto rendimiento para análisis de JSON.