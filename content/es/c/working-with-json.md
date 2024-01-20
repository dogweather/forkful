---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Trabajar con JSON en C significa manejar datos en un formato ligero de intercambio de datos. Los programadores lo hacen porque JSON es fácil de entender para humanos y máquinas, además de ser el estándar de facto para APIs y configuraciones en web.

## Cómo:

```c
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main() {
    // Crear un objeto JSON
    json_object *nuevo_json = json_object_new_object();
    
    // Añadir datos al objeto JSON
    json_object_object_add(nuevo_json, "nombre", json_object_new_string("Juan"));
    json_object_object_add(nuevo_json, "edad", json_object_new_int(30));
    
    // Convertir a cadena de texto y mostrar
    const char *cadena_json = json_object_to_json_string(nuevo_json);
    printf("JSON string: %s\n", cadena_json);

    // Liberar memoria
    json_object_put(nuevo_json);
    
    return 0;
}
```

**Salida esperada**

```
JSON string: {"nombre": "Juan", "edad": 30}
```

## Deep Dive

JSON (JavaScript Object Notation) nació en 2001, diseñado por Douglas Crockford. Alternativas incluyen XML y YAML, pero JSON gana en simplicidad y legibilidad. C no tiene soporte nativo para JSON, así que usamos librerías como `json-c` o `jansson` para analizar y generar JSON. Estas bibliotecas manejan el mapeo entre JSON y las estructuras de datos en C, lo cual puede ser complejo debido a la tipificación estática del lenguaje.

## Ver También

- Documentación oficial de `json-c`: https://json-c.github.io/json-c/
- Tutorial de `jansson`: https://jansson.readthedocs.io/en/latest/tutorial.html
- Especificación oficial de JSON: https://www.json.org/json-es.html