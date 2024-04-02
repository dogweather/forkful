---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:17.625348-07:00
description: "TOML (Tom's Obvious, Minimal Language o El Lenguaje M\xEDnimo y Obvio\
  \ de Tom) es un formato de archivo de configuraci\xF3n f\xE1cil de leer debido a\
  \ su clara\u2026"
lastmod: '2024-03-13T22:44:59.571367-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language o El Lenguaje M\xEDnimo y Obvio de\
  \ Tom) es un formato de archivo de configuraci\xF3n f\xE1cil de leer debido a su\
  \ clara\u2026"
title: Trabajando con TOML
weight: 39
---

## Qué y por qué?

TOML (Tom's Obvious, Minimal Language o El Lenguaje Mínimo y Obvio de Tom) es un formato de archivo de configuración fácil de leer debido a su clara semántica. Los programadores lo usan para archivos de configuración en aplicaciones porque su simplicidad y legibilidad para humanos lo hacen una excelente elección sobre formatos como XML o JSON en ciertos contextos.

## Cómo hacerlo:

Para trabajar con TOML en C, primero necesitas una biblioteca capaz de analizar archivos TOML, ya que la biblioteca estándar de C no incluye esta funcionalidad. Una opción popular es `tomlc99`, un analizador TOML ligero para C99. Aquí tienes una guía rápida para leer un simple archivo de configuración TOML:

Primero, asegúrate de tener `tomlc99` instalado y correctamente vinculado en tu proyecto.

**Archivo TOML de muestra (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Código C para analizar este archivo:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("No se puede abrir el archivo");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error al analizar el archivo\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Servidor de base de datos: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Puerto %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Salida:**
```
Servidor de base de datos: "192.168.1.1"
Puerto 0: 8001
Puerto 1: 8001
Puerto 2: 8002
```

## Análisis Profundo

TOML fue creado por Tom Preston-Werner, cofundador de GitHub, como respuesta a las limitaciones que percibía en otros formatos de archivo de configuración. Su objetivo es ser directo y no ambiguo, tanto para humanos como para computadoras, para leer y escribir sin necesidad de reglas de análisis complejas. En el ecosistema de C, TOML no es un ciudadano de primera clase como podría serlo en lenguajes de programación de alto nivel como Rust con su `serde_toml` o Python con `toml`, que tienen bibliotecas con soporte nativo. Sin embargo, los desarrolladores de C necesitan confiar en bibliotecas externas como `tomlc99`, pero esto es típico dada la énfasis de C en el minimalismo y el rendimiento.

Aunque TOML es elogiado por su claridad, al elegir un formato de archivo de configuración, es vital considerar las necesidades del proyecto. En escenarios que requieren estructuras más complejas o interactividad con APIs web, JSON o incluso YAML podrían ofrecer un mejor ajuste a pesar de su mayor complejidad. TOML brilla en configuraciones donde la legibilidad y simplicidad son primordiales, no necesariamente donde se necesitan las estructuras de datos más avanzadas.
