---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Trabajar con archivos CSV (valores separados por comas) significa manipular datos en un formato comúnmente usado para intercambios de información simple y masiva. Los programadores lo hacen para importar, exportar y procesar datos de manera rápida y compatible entre distintas aplicaciones.

## Cómo hacerlo:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char buffer[1024];
    FILE *archivo = fopen("datos.csv", "r");
    if (!archivo) {
        printf("Error al abrir el archivo\n");
        return 1;
    }

    while (fgets(buffer, 1024, archivo)) {
        char *valor = strtok(buffer, ",");
        while (valor) {
            printf("%s\n", valor);
            valor = strtok(NULL, ",");
        }
    }
    fclose(archivo);
    return 0;
}
```

Ejemplo de salida:

```
Nombre
Edad
Ciudad
Juan
25
Madrid
```

## Análisis Profundo:

El formato CSV data desde los primeros días de la informática personal. Surgió como una manera sencilla de representar tablas de datos. Si bien JSON o XML ofrecen estructuras más ricas, CSV se mantiene por su simplicidad y bajo costo de procesamiento. Implementarlo en C requiere manejo manual de archivos y cadenas de texto, lo que implica dominar `fopen`, `fgets`, `strtok` y manejo de punteros.

## Ver También:

- [RFC 4180](https://tools.ietf.org/html/rfc4180), que describe el formato CSV estándar.
- Librería [libcsv](http://sourceforge.net/projects/libcsv/), para facilitar la manipulación de CSV en C.
- Documentación de C en [cppreference.com](https://en.cppreference.com/w/c), útil para profundizar en funciones de manejo de archivos y cadenas.
