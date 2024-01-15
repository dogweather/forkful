---
title:                "Trabajando con yaml"
html_title:           "C: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML 
Si estás buscando un formato de datos ligero y fácil de leer, entonces YAML es la elección ideal para ti. Con su sintaxis simple y legible, YAML es ampliamente utilizado para la configuración de aplicaciones y archivos de datos.

## Cómo usar YAML en tu código

Para comenzar a trabajar con YAML en tu código C, primero deberás incluir la biblioteca "yaml.h". Luego, puedes utilizar la función "yaml_emit" para crear tu documento YAML y la función "yaml_load" para cargar datos de un archivo YAML existente.

```C
#include <yaml.h>

//Ejemplo de creación de un documento YAML
yaml_document_t document;
yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

//Agregando un valor al documento YAML
yaml_node_t *node = yaml_document_add_scalar(&document, NULL, "¡Hola mundo!");
```

El resultado será un documento YAML que contenga el valor "¡Hola mundo!". Ahora, si queremos cargar los datos de un archivo YAML existente, podemos hacerlo de la siguiente manera:

```C
FILE *archivo;
archivo = fopen("datos.yaml", "rb");
...
void *parser = yaml_parser_initialize(buffer, sizeof(buffer));
yaml_parser_set_input_file(parser, file);
yaml_parser_parse(parser, &document);
```

¡Y listo! Ahora tienes acceso a los datos del archivo YAML en tu código C.

## Profundizando en YAML

Además de la creación y la carga de datos, YAML también ofrece otras funciones útiles, como la manipulación de nodos, la serialización de datos y la validación de documentos YAML. Puedes explorar todas estas funciones en la documentación oficial de YAML para C.

## Ver también

- [Documentación oficial de YAML para C](https://yaml.org/spec/1.2/spec.html)
- [Ejemplos de código YAML para C](https://github.com/yaml/libyaml/tree/master/tests)
- [Tutorial introductorio de YAML para C](https://www.melvinvivas.com/yaml-c/)