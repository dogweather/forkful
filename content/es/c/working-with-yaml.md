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

# Qué y por qué?

Trabajar con YAML (YAML Ain't Markup Language) implica usar un formato de datos basado en texto para representar estructuras de datos. Los programadores lo utilizan porque es un formato fácil de leer y escribir, lo que lo hace ideal para configuraciones de aplicaciones y archivos de datos.

# Cómo hacerlo:

```C
// Incluye la biblioteca YAML
#include <yaml.h>

// Crea un documento YAML
yaml_document_t document;
yaml_document_initialize(&document, NULL, 0, 0);

// Agrega un par clave-valor al documento
yaml_node_t *root = yaml_document_get_root_node(&document);
yaml_node_t *key = yaml_document_add_scalar(&document, NULL, "key");
yaml_node_t *value = yaml_document_add_scalar(&document, NULL, "value");
yaml_node_pair_t *pair = yaml_document_add_mapping_pair(&document, root, key, value);

// Imprime el documento
yaml_document_dump(&document, stdout);

// Limpia el documento
yaml_document_delete(&document);
```

Salida: ```key: value```

# Inmersión profunda:

- YAML fue diseñado originalmente para ser un formato de serialización de datos para lenguajes de programación, pero también se puede utilizar para archivos de configuración y otros usos.
- Alternativas populares a YAML incluyen JSON y XML.
- Para implementar YAML en un proyecto de C, se puede utilizar una biblioteca como libyaml.

# Ver también:

- [Sitio oficial de YAML](https://yaml.org/)
- [Documentación de la biblioteca libyaml](https://pyyaml.org/wiki/LibYAML)
- [Especificación de YAML](https://yaml.org/spec/)