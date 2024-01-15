---
title:                "Trabajando con json"
html_title:           "C: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON?

JSON (JavaScript Object Notation) es un formato de datos popular utilizado en el desarrollo de aplicaciones web y móviles. Al usar JSON, puedes almacenar y transferir datos de manera eficiente entre diferentes sistemas y plataformas. Además, es un formato legible para los humanos y fácilmente manipulable por las computadoras, lo cual lo hace una opción ideal para trabajar con datos en el lenguaje de programación C.

## Cómo trabajar con JSON

Para trabajar con JSON en C, primero necesitas un parser de JSON. Hay varias bibliotecas disponibles, pero en este artículo vamos a utilizar json-c, una biblioteca de código abierto. A continuación, te mostraremos un ejemplo de cómo leer y escribir datos JSON utilizando esta biblioteca:

```
#include <stdio.h>
#include <json-c/json.h>

int main() {
  // Crear una estructura JSON básica
  json_object *mi_json = json_object_new_object();
  json_object_object_add(mi_json, "nombre", json_object_new_string("Juan"));
  json_object_object_add(mi_json, "edad", json_object_new_int(25));

  // Imprimir el JSON en consola
  printf("%s\n", json_object_to_json_string(mi_json));

  // Actualizar la edad en el JSON
  json_object_object_add(mi_json, "edad", json_object_new_int(26));

  // Imprimir el JSON actualizado
  printf("%s\n", json_object_to_json_string(mi_json));

  // Liberar la memoria asignada al JSON
  json_object_put(mi_json);

  return 0;
}
```

**Salida:**
```
{"nombre": "Juan", "edad": 25}
{"nombre": "Juan", "edad": 26}
```

¡Y eso es todo! Con tan solo unas pocas líneas de código, podemos crear, leer y actualizar un objeto JSON en C.

## Inmersión profunda en JSON

Una vez que tengas dominado el manejo básico de JSON en C, puedes profundizar en su uso y explorar sus funciones más avanzadas. Puedes leer y escribir archivos JSON, trabajar con arrays y objetos anidados, y utilizar funciones de validación para asegurarte de que tus datos están en el formato correcto. También puedes aprender acerca de la manipulación de JSON en tiempo real, permitiendo una comunicación más fluida entre el lado del servidor y el lado del cliente en tus aplicaciones web.

## Ver también

- [json-c en GitHub](https://github.com/json-c/json-c)
- [Tutorial de json-c en CodeProject](https://www.codeproject.com/Tips/828139/Using-json-c-JSON-parser-and-generator-with-cplusp)
- [Documentación oficial de JSON](https://www.json.org/json-es.html)