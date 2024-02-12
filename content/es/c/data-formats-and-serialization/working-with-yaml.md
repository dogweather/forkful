---
title:                "Trabajando con YAML"
aliases:
- /es/c/working-with-yaml.md
date:                  2024-02-03T18:13:08.509909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

YAML, que significa "YAML Ain't Markup Language" (YAML No es un Lenguaje de Marcado), es un estándar de serialización de datos legible por humanos que se puede utilizar para todo tipo de aplicaciones, desde archivos de configuración hasta almacenamiento de datos. Los programadores suelen trabajar con YAML cuando necesitan un formato fácil de leer y escribir para archivos de configuración o intercambio de datos entre lenguajes y sistemas.

## Cómo hacerlo:

Trabajar con YAML en C requiere una biblioteca, ya que la biblioteca estándar de C no proporciona soporte directo para el análisis o serialización de YAML. Una de las bibliotecas YAML más populares para C es `libyaml`, que ofrece interfaces de bajo y alto nivel para analizar y emitir YAML. A continuación, se muestra un ejemplo de cómo analizar un archivo YAML simple usando `libyaml`:

**Primero**, necesitas instalar la biblioteca `libyaml`. Si estás en un sistema similar a Unix, usualmente puedes instalarlo a través de tu gestor de paquetes. Por ejemplo, en Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Luego**, considera un archivo YAML simple llamado `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Aquí hay** un ejemplo básico de cómo analizar este archivo YAML en C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("¡Falló al inicializar el analizador de YAML!\n", stderr);

    if (fh == NULL)
        fputs("¡Falló al abrir archivo!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Valor: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Este sencillo programa abre un archivo YAML, inicializa el analizador de YAML y lee el archivo, imprimiendo los valores escalares (en este ejemplo, los campos de nuestro YAML simple). Ten en cuenta que la verificación de errores es mínima en este ejemplo simple y debería ser más robusta en el código de producción.

Ejecutar el programa con nuestro `config.yaml` generará la salida:

```plaintext
Valor: John Doe
Valor: 29
Valor: false
```

## Análisis Profundo

YAML fue lanzado por primera vez en 2001 y diseñado para ser más legible y amigable para el usuario que otros formatos de serialización de datos como XML o JSON, tomando prestado de varios lenguajes como C, Perl y Python para su filosofía de diseño. A pesar de sus ventajas en legibilidad y facilidad de modificación humana, YAML puede ser complejo de analizar programáticamente debido a su dependencia de la indentación y su amplio conjunto de características, incluyendo referencias y tipos personalizados.

Aunque `libyaml` proporciona un acceso robusto de bajo nivel para analizar y emitir YAML en C, puede ser engorroso para tareas simples debido a su API verbosa. Por estas razones, algunos programadores prefieren usar bibliotecas de más alto nivel o incluso otros formatos de serialización de datos como JSON cuando trabajan en C, especialmente cuando la eficiencia del análisis con un mínimo de código es una prioridad. Sin embargo, YAML sigue siendo una opción popular para archivos de configuración y situaciones donde la legibilidad humana es primordial. Alternativas como TinyYAML o incrustar un intérprete de alto nivel (por ejemplo, incrustar Python o Lua) podrían proporcionar más conveniencia para aplicaciones específicas, equilibrando entre facilidad de uso y necesidades de rendimiento.
