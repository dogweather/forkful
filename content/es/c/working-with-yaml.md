---
title:                "C: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML en C

En el mundo de la programación, existen muchos formatos de datos diferentes que se utilizan para almacenar y transferir información. Uno de estos formatos es YAML, el cual es especialmente útil para crear archivos de configuración. Al trabajar con YAML en C, podrás tener un control más preciso sobre la forma en que se almacenan y se interpretan los datos en tus aplicaciones.

## Cómo trabajar con YAML en C

Para trabajar con YAML en C, es necesario utilizar una biblioteca que permita la manipulación de archivos YAML. Una de las opciones más populares y ampliamente utilizadas es libyaml, la cual se puede descargar e instalar fácilmente en tu sistema.

Una vez que hayas instalado libyaml, puedes comenzar a utilizarlo en tus proyectos de C mediante las funciones y estructuras proporcionadas por la biblioteca. A continuación, se muestra un ejemplo sencillo de cómo leer y escribir datos en un archivo YAML utilizando libyaml:

```C
#include <yaml.h>

int main(void) {

    // Crear un objeto para almacenar los datos
    yaml_document_t doc;
    
    // Inicializar el objeto
    yaml_document_initialize(&doc, NULL, NULL, NULL, 0, 0);
    
    // Crear un nodo de tipo mapa
    yaml_node_t *map = yaml_document_add_mapping(&doc, NULL, YAML_ANY_MAPPING_STYLE);
    
    // Agregar pares clave-valor al mapa
    yaml_node_t *key = yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"nombre", -1);
    yaml_node_t *value = yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"Juan", -1);
    yaml_document_append_mapping_pair(&doc, map, key, value);
    
    key = yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"edad", -1);
    value = yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"30", -1);
    yaml_document_append_mapping_pair(&doc, map, key, value);
    
    // Guardar el archivo YAML
    FILE *fp = fopen("ejemplo.yaml", "wb");
    yaml_document_emit(&doc, fp);
    fclose(fp);
    
    // Liberar la memoria utilizada por el objeto
    yaml_document_delete(&doc);
    
    return 0;
}
```

Este código creará un archivo YAML llamado "ejemplo.yaml" que contenga los datos almacenados en el objeto `map`. A continuación, se muestra cómo se vería la estructura del archivo:

```yaml
nombre: Juan
edad: 30
```

Ahora, para leer y manipular un archivo YAML existente, se puede utilizar el siguiente código:

```C
#include <yaml.h>

int main(void) {

    // Crear un objeto para almacenar los datos
    yaml_document_t doc;
    
    // Inicializar el objeto
    yaml_document_initialize(&doc, NULL, NULL, NULL, 0, 0);
    
    // Cargar el archivo YAML
    FILE *fp = fopen("ejemplo.yaml", "rb");
    yaml_document_load(&doc, fp);
    fclose(fp);
    
    // Obtener el nodo raíz
    yaml_node_t *root = yaml_document_get_root_node(&doc);
    
    // Obtener el primer elemento del mapa
    yaml_node_pair_t *pair = root->data.mapping.pairs.start;
    
    // Obtener la clave y valor del primer elemento
    yaml_node_t *key = yaml_document_get_node(&doc, pair->key);
    yaml_node_t *value = yaml_document_get_node(&doc, pair->value);
    
    // Imprimir la clave y valor
    printf("%s: %s\n", key->data.scalar.value, value->data.scalar.value);
    
    // Liberar la memoria utilizada por el objeto
    yaml_document_delete(&doc);
    
    return 0;
}
```

Este código leerá el archivo YAML creado anteriormente e imprimirá los datos almacenados dentro de él.

## Profundizando en YAML

Además de las funciones básicas de lectura y escritura de archivos YAML, libyaml también ofrece opciones avanzadas para manipular los datos, como la posibilidad de trabajar con múltiples documentos en un solo archivo YAML.

Para obtener más información sobre cómo trabajar con YAML en C, puede consultar la documentación oficial de libyaml y explorar sus diferentes funciones y estructuras.

## Vea también

- [Documentación oficial de libyaml] (https://pyyaml.org/wiki/LibYAML)
- [Ejemplos de código de libyaml] (https://github.com/yaml