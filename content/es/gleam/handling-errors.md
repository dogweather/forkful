---
title:                "Manejo de errores"
date:                  2024-01-26T00:52:01.449102-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Manejar errores se trata de anticiparse a que las cosas salgan mal en tu código y gestionar esas situaciones de manera elegante. Los programadores hacen esto porque mantiene las aplicaciones robustas y fáciles de usar, incluso cuando se enfrentan a lo inesperado.

## Cómo hacerlo:
En Gleam, a menudo harás uso del tipo `Result` para el manejo de errores. Es un enum con dos variantes: `Ok` (para éxito) y `Error` (para fallos). Aquí tienes un ejemplo simple:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("¡Ups! Se rompió.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(mensaje) => {
      io.println(mensaje)
      0
    } 
  }
}
```

Si ejecutas `main` con `might_fail(False)`, devolverá `42`. Si pasas `True`, imprimirá "¡Ups! Se rompió." y retornará `0`.

## Profundización
El enfoque de Gleam para el manejo de errores está influenciado por sus raíces en Erlang. Históricamente, Erlang utiliza una filosofía de "dejarlo fallar", confiando en árboles de supervisión para gestionar las fallas de los procesos. Sin embargo, cuando estás escribiendo código en Gleam que no está dentro de un proceso destinado a ser supervisado, como dentro de una función de biblioteca, querrás manejar los errores explícitamente.

Alternativas al uso de `Result` incluyen el uso del tipo `Option` para casos donde algo podría ser `None` (nada) o `Some` (algo), pero estos no llevan información de error. Para señalizar errores a través de los límites de procesos, podrías usar los mecanismos de paso de mensajes de Erlang.

El manejo de errores de Gleam refleja un estilo de programación funcional, donde los efectos secundarios (como los errores) se gestionan con tipos y coincidencia de patrones, proporcionando claridad y previsibilidad en la gestión de errores.

## Ver También
- [Manejo de Errores en Erlang](http://erlang.org/doc/reference_manual/errors.html)
