---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Escribir en el error estándar (stderr) permite separar la salida de errores de la salida normal (stdout). Los programadores lo hacen para diagnosticar problemas y depurar su código más fácilmente, manteniendo los errores visibles y aparte de la salida principal.

## Cómo hacerlo:
En Gleam, puedes usar la función `io.println` de la biblioteca estándar para escribir en stderr. Aquí un ejemplo:

```gleam
import gleam/io

pub fn main() {
  let _ = io.println("Esto es un mensaje de error", to: .standard_error)
}
```

Salida de muestra:

```
Esto es un mensaje de error
```

## Profundización
Históricamente, la separación de stdout y stderr proviene de Unix. Alternativas incluyen escribir en archivos de registro o utilizar herramientas de monitoreo. En Gleam, el manejo de stderr es similar a otros lenguajes de Erlang VM, aprovechando las capacidades de concurrencia y tolerancia a fallos del sistema.

## Ver también
- Una explicación sobre stdout y stderr en Unix: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
- Documentación de Erlang sobre errores y manejo de errores: [https://erlang.org/doc/apps/stdlib/io_protocol.html](https://erlang.org/doc/apps/stdlib/io_protocol.html)
