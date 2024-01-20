---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:56:06.237232-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? | ¿Qué y Por Qué?
Verificar si un directorio existe asegura que nuestro programa acceda solo a rutas válidas. Hacemos esto para evitar errores de lectura/escritura y para confirmar que los datos que necesitamos están donde esperamos.

## How to: | Cómo hacerlo:
```gleam
import gleam/io
import gleam/erlang
import gleam/erlang/file

pub fn check_dir_exists(path: String) -> Bool {
  // Usa 'erlang.file:read_dir' para intentar leer el directorio
  case file.read_dir(path) {
    Ok(_) -> true  // El directorio existe
    Error(_) -> false  // No existe o hay un error de permisos
  }
}

// Uso
fn main() {
  let dir_exists = check_dir_exists("some/path/to/directory")
  io.debug(dir_exists)  // Imprime 'True' si existe, 'False' si no
}
```

## Deep Dive | Inmersión Profunda
Antes, para verificar la existencia de directorios, teníamos que depender del Sistema Operativo o de llamadas a comandos del shell. En Gleam, abstraemos esta funcionalidad con `gleam/erlang/file`. Otros lenguajes tienen sus métodos; por ejemplo, Python usa `os.path.exists` y Node.js `fs.existsSync`.

El código muestra como usar el módulo `file` de la biblioteca estándar Erlang. El manejo de errores es crucial; un `Error(_)` puede indicar que el directorio no existe o que ocurrió un problema de permisos.

Alternativamente, si estás construyendo una aplicación robusta que necesita manejar muchos escenarios de sistema de archivos, considera usar una biblioteca externa que ofrezca más funciones y detalles.

## See Also | Ver También
- Documentation of Erlang's `file` module: [Erlang -- file](http://erlang.org/doc/man/file.html)
- The `gleam/erlang/file` docs: [Gleam stdlib](https://hexdocs.pm/gleam_stdlib/)