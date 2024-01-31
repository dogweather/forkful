---
title:                "Trabajando con TOML"
date:                  2024-01-26T04:19:27.378381-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-toml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
TOML es un lenguaje de serialización de datos diseñado para ser fácil de leer y escribir. Los programadores lo utilizan para archivos de configuración, almacenamiento de datos simples e intercambio de datos entre lenguajes debido a su claridad y amigabilidad con los humanos.

## Cómo hacerlo:
Vamos a analizar un archivo de configuración TOML en C utilizando la biblioteca "tomlc99". Primero, instala la biblioteca. Luego, crea un `config.toml`:

```toml
title = "Ejemplo de TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Ahora, analízalo en C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Error: no se puede abrir el archivo de configuración\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Error: %s\n", errbuf);
        return 1;
    }

    printf("Título: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Nombre del Propietario: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Salida de muestra:
```
Título: "Ejemplo de TOML"
Nombre del Propietario: "Tom Preston-Werner"
```

## Profundización
TOML, que significa Tom's Obvious, Minimal Language (el lenguaje obvio y minimalista de Tom), fue creado por Tom Preston-Werner en 2013. Sirve como una alternativa más sencilla a formatos como XML y YAML, enfocándose en ser más legible y escribible por humanos. Aunque JSON es otra alternativa, TOML mantiene una estructura que es más fácil de analizar visualmente por los humanos, lo cual es una de las principales razones de su adopción en archivos de configuración.

En C, trabajar con TOML implica elegir una biblioteca de análisis ya que el lenguaje no lo soporta de forma nativa. Bibliotecas como "tomlc99" son compatibles con C99 y proporcionan una API para decodificar texto TOML. Al considerar el rendimiento, el manejo adecuado de errores y la gestión de la memoria son cruciales, ya que C no tiene recolección de basura integrada.

## Ver También:
1. Especificación de TOML: [https://toml.io/en/](https://toml.io/en/)
2. Repositorio de GitHub de tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Comparando Formatos de Serialización de Datos: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
