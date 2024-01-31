---
title:                "Trabajando con YAML"
date:                  2024-01-19
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
YAML es un lenguaje de serialización legible para humanos, usado para configuración, archivos de datos y más. Programadores usan YAML por su facilidad de lectura y compatibilidad con varios lenguajes de programación.

## Cómo Hacer:
Para trabajar con YAML en C, necesitarás una biblioteca como `libyaml`. Aquí veremos cómo leer y parsear un archivo YAML simple.

```C
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    if(fh == NULL) {
        printf("No se pudo abrir el archivo config.yaml\n");
        return 1;
    }

    if(!yaml_parser_initialize(&parser)) {
        printf("Inicialización del parseador fallida\n");
        return 2;
    }

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event)) {
            printf("Error al parsear YAML\n");
            break;
        }

        // Haz algo con event.type o event.data aquí

        if (event.type == YAML_STREAM_END_EVENT) {
            break;
        }

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);

    return 0;
}
```

Este código abre un archivo `config.yaml`, inicializa un parser de YAML y lee el archivo, evento por evento. El procesamiento de los datos depende de tu aplicación.

## Deep Dive:
YAML, acrónimo de "YAML Ain't Markup Language" (un juego de palabras que significa "YAML no es un lenguaje de marcado"), emergió a principios de los años 2000. Se considera más simple y legible que alternativas como XML o JSON. La simplicidad es relativa; los archivos grandes de YAML pueden ser complicados. Librerías como `libyaml` (para C) ayudan a manejar la complejidad del parseo y la generación de archivos YAML.

## See Also:
- Librería oficial de YAML para C: http://pyyaml.org/wiki/LibYAML
- Especificación de YAML: https://yaml.org/spec/1.2/spec.html
- Tutorial interactivo de YAML: https://learnxinyminutes.com/docs/yaml/
