---
title:                "Trabajando con TOML"
date:                  2024-01-26T04:21:57.982999-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con TOML implica analizar y codificar archivos TOML (Tom's Obvious, Minimal Language) en Go. Los programadores optan por TOML por su legibilidad y fácil mapeo a estructuras de datos, siendo una opción sólida para configuraciones.

## Cómo hacerlo:
Para trabajar con TOML en Go, típicamente usarás una biblioteca como `BurntSushi/toml`. Aquí tienes una rápida mirada a cómo analizar un archivo de configuración TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Ejemplo de `config.toml`:

```Toml
title = "Ejemplo TOML"
[owner]
name = "Tom Preston-Werner"
```

Salida de muestra:

```
Title: Ejemplo TOML, Owner: Tom Preston-Werner
```

## Profundizando
TOML, introducido por Tom Preston-Werner en 2013, fue diseñado para ser un formato de archivo de configuración mínimo que es fácil de leer debido a su clara semántica. Los desarrolladores de Go a menudo usan TOML para configuración sobre alternativas como JSON o YAML por su sencillez y capacidad para representar jerarquías complejas con simplicidad.

En comparación con YAML, que tiene características complejas y posibles preocupaciones de seguridad, el diseño plano de TOML reduce la complejidad y los errores inducidos por typos. Y a diferencia de JSON, TOML admite comentarios, lo que facilita la explicación de configuraciones en línea.

Cuando trabajas con TOML en Go, hay matices a considerar. Las etiquetas de estructura pueden personalizar cómo tus estructuras se mapean a estructuras TOML, y también debes estar consciente de cómo se analizan los arreglos de TOML y las tablas en línea en rebanadas y mapas de Go.

## Ver También
- Especificación de TOML: https://toml.io/en/
- Biblioteca BurntSushi/toml: https://github.com/BurntSushi/toml
- Una comparación de formatos de archivos de configuración: https://www.redhat.com/sysadmin/yaml-toml-json-differences