---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:26.596924-07:00
description: "Iniciar un nuevo proyecto en Go implica configurar un espacio de trabajo\
  \ e inicializarlo con los m\xF3dulos de Go necesarios. Los programadores hacen esto\u2026"
lastmod: '2024-03-11T00:14:32.338728-06:00'
model: gpt-4-0125-preview
summary: "Iniciar un nuevo proyecto en Go implica configurar un espacio de trabajo\
  \ e inicializarlo con los m\xF3dulos de Go necesarios. Los programadores hacen esto\u2026"
title: Iniciando un nuevo proyecto
---

{{< edit_this_page >}}

## Qué y Por Qué?

Iniciar un nuevo proyecto en Go implica configurar un espacio de trabajo e inicializarlo con los módulos de Go necesarios. Los programadores hacen esto para organizar el código, gestionar dependencias de manera eficaz y facilitar los procesos de construcción. Es fundamental para crear software en Go que sea escalable y mantenible.

## Cómo hacerlo:

Primero, asegúrate de tener Go instalado ejecutando `go version` en tu terminal. Deberías ver la versión de Go que has instalado como salida. A continuación, comencemos un nuevo proyecto. Navega a tu espacio de trabajo y ejecuta:

```shell
mkdir hola-mundo
cd hola-mundo
```

Esto crea y te mueve a un nuevo directorio para tu proyecto. Ahora, inicializa el módulo:

```shell
go mod init ejemplo.com/hola-mundo
```

Reemplaza `ejemplo.com/hola-mundo` con la ruta de tu módulo. Este comando crea un archivo `go.mod` en tu directorio, señalando el inicio de un nuevo módulo de Go. Así es como podría verse `go.mod`:

```plaintext
module ejemplo.com/hola-mundo

go 1.18
```

`go.mod` rastrea las dependencias de tu proyecto. Ahora, crea un archivo `main.go`:

```shell
touch main.go
```

Abre `main.go` en tu editor favorito y añade el siguiente código para imprimir "¡Hola, Mundo!":

```go
package main

import "fmt"

func main() {
    fmt.Println("¡Hola, Mundo!")
}
```

Para ejecutar tu programa, vuelve al terminal y ejecuta:

```shell
go run main.go
```

Deberías ver:

```plaintext
¡Hola, Mundo!
```

¡Felicidades! Acabas de iniciar un nuevo proyecto en Go y ejecutar tu primer programa en Go.

## Análisis profundo

La iniciativa de introducir módulos como el estándar para la gestión de dependencias en Go fue un cambio significativo en el ecosistema de Go, oficialmente adoptado en Go 1.11. Antes de los módulos, los desarrolladores de Go confiaban en la variable de entorno GOPATH para gestionar dependencias, lo cual era menos intuitivo y a menudo conducía al infame "infierno de dependencias".

Los módulos proporcionan una manera encapsulada de gestionar las dependencias del proyecto, la versioning, y son un paso hacia la realización de proyectos de Go más autocontenidos y portátiles. Cada módulo especifica sus dependencias que Go rastrea en el archivo `go.mod`, simplificando la gestión de dependencias a través de diferentes entornos y etapas de desarrollo.

Sin embargo, vale la pena mencionar que, aunque los módulos de Go son ahora el estándar, algunos proyectos heredados todavía podrían usar GOPATH. Para la mayoría de los nuevos proyectos, los módulos ofrecen un sistema de gestión más sencillo y efectivo, pero entender GOPATH puede ser útil para mantener o contribuir a bases de código Go antiguas.

En términos de alternativas, aunque los módulos de Go son ahora el estándar de facto, la comunidad de Go ha experimentado con otras herramientas de gestión de dependencias como `dep` en el pasado. Sin embargo, estas han sido en gran medida superadas por el soporte oficial de módulos integrado en la cadena de herramientas de Go.
