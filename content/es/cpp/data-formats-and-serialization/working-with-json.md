---
title:                "Trabajando con JSON"
aliases:
- /es/cpp/working-with-json.md
date:                  2024-02-03T19:21:45.728778-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

JSON (JavaScript Object Notation) es un formato ligero para almacenar y transportar datos, lo que lo convierte en un excelente medio para el intercambio de datos entre servidores y aplicaciones web. Los programadores usan JSON debido a su fácil legibilidad por humanos y sencilla interpretabilidad por máquinas, especialmente cuando se trabaja en aplicaciones que requieren intercambio de datos a través de internet o configuraciones.

## Cómo hacerlo:

En C++, no hay soporte nativo para JSON, pero bibliotecas de terceros como nlohmann/json hacen que sea directo. Aquí te mostramos cómo usarlo para tareas básicas:

Primero, asegúrate de tener instalada la biblioteca. Si usas un gestor de paquetes como vcpkg o Conan, puedes agregar fácilmente `nlohmann/json` a tu proyecto.

### Analizando JSON de un string

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Datos JSON como un string
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Parsear el string JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Accediendo a los datos
    std::cout << "Nombre: " << jsonObject["name"] << "\n"
              << "Edad: " << jsonObject["age"] << "\n"
              << "Ciudad: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Salida de muestra:**

```
Nombre: John
Edad: 30
Ciudad: New York
```

### Generando JSON

Crear datos JSON es igual de sencillo; simplemente asignas valores a un objeto `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Creando un objeto JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Convertir objeto JSON a string e imprimir
    std::string jsonString = jsonObject.dump(4); // Argumento 4 para impresión bonita
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Salida de muestra:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Estos ejemplos demuestran la funcionalidad principal para trabajar con JSON en C++ usando la biblioteca `nlohmann/json`. Con estos conceptos básicos, puedes analizar y generar JSON para varias aplicaciones, desde archivos de configuración hasta intercambio de datos en aplicaciones en red.
