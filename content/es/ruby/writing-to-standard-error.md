---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escribir en el error estándar es mostrar mensajes de error en la consola. Los programadores hacen esto para separar los errores del flujo de salida normal, facilitando el debugging y la gestión de logs.

## How to:
Para escribir en el error estándar en Ruby, usa `$stderr.puts`. Ejemplo:

```Ruby
$stderr.puts "Error: Operación inválida."
```

Salida al ejecutarlo:

```
Error: Operación inválida.
```

Puedes redirigir la salida a un archivo:

```Ruby
$stderr.reopen("errores.log", "w")
$stderr.puts "Failed to open file."
```

Ahora el mensaje está en `errores.log`, no en la pantalla.

## Deep Dive
Históricamente, la distinción entre salida estándar y error estándar viene de UNIX. `$stdout` es para resultados normales, `$stderr` para errores y diagnostic messages. Existen otras formas como `STDERR.puts` o `warn`. En cuanto a detalles de implementación, `$stderr` es un global predefinido que representa una instancia de `IO` dirigida al error estándar.

## See Also
- Documentación de Ruby IO: https://ruby-doc.org/core/IO.html
- Guía sobre STDERR, STDOUT y STDIN: https://www.jstorimer.com/blogs/workingwithcode/7766119-when-to-use-stderr-instead-of-stdout
- Explicación sobre la salida estándar y de error en UNIX: http://www.tldp.org/LDP/abs/html/io-redirection.html
