---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:38.709675-07:00
description: "TOML (Tom's Obvious, Minimal Language) es un formato de archivo de configuraci\xF3\
  n que es f\xE1cil de leer debido a su sintaxis simple. Los programadores usan\u2026"
lastmod: '2024-03-13T22:44:58.495788-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) es un formato de archivo de configuraci\xF3\
  n que es f\xE1cil de leer debido a su sintaxis simple. Los programadores usan\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

TOML (Tom's Obvious, Minimal Language) es un formato de archivo de configuración que es fácil de leer debido a su sintaxis simple. Los programadores usan TOML para configurar ajustes de aplicaciones y dependencias debido a su claridad y mapeo directo a estructuras de datos, lo que lo convierte en una opción popular en muchos proyectos de Go para configurar y gestionar configuraciones.

## Cómo hacerlo:

Para comenzar a trabajar con TOML en Go, primero necesitas incluir una biblioteca que pueda parsear archivos TOML, ya que la biblioteca estándar de Go no soporta nativamente TOML. El paquete `BurntSushi/toml` es una elección popular para esto. Primero, asegúrate de instalarlo:

```bash
go get github.com/BurntSushi/toml
```

Aquí tienes un ejemplo simple de cómo usarlo. Considera que tienes un archivo de configuración llamado `config.toml` con el siguiente contenido:

```toml
title = "Ejemplo TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Ahora, necesitas crear una estructura de Go que refleje la estructura de TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Habilitado    bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Título: %s\n", config.Title)
    fmt.Printf("Servidor de base de datos: %s\n", config.Database.Server)
}
```

Salida de muestra:

```
Título: Ejemplo TOML
Servidor de base de datos: 192.168.1.1
```

## Inmersión Profunda

TOML fue creado por Tom Preston-Werner, uno de los cofundadores de GitHub, para ofrecer un formato de archivo de configuración sencillo que pueda mapearse fácilmente a una tabla hash y sea comprendido a simple vista sin conocimiento previo del formato. Se diferencia de JSON o YAML, que, aunque también son ampliamente utilizados, pueden ser menos amigables para los humanos en archivos de configuración debido a problemas con las llaves, comillas e indentación.

El paquete `BurntSushi/toml` en Go es una biblioteca robusta que permite no solo decodificar sino también codificar archivos TOML, lo que lo convierte en una opción versátil para aplicaciones que necesitan leer y escribir archivos de configuración en este formato. Sin embargo, se debe tener en cuenta que con el avance de las tecnologías y la introducción de nuevas versiones de Go, han surgido alternativas como `pelletier/go-toml`, que ofrecen un mejor rendimiento y características adicionales como manipulación de árboles y soporte de consultas.

Aunque TOML es una excelente elección para muchas aplicaciones, dependiendo de la complejidad de la configuración de la aplicación y de las preferencias personales o del equipo, otros formatos como YAML o JSON podrían ser más adecuados, especialmente si la configuración requiere estructuras de datos más complejas que la naturaleza verbosa de TOML no pueda capturar elegantemente. No obstante, para configuraciones sencillas, legibles y fácilmente editables, TOML, junto con el sólido sistema de tipos de Go y las bibliotecas mencionadas, es una elección excelente.
