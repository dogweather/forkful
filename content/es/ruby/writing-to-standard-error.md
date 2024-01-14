---
title:    "Ruby: Escribiendo en el error estándar"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## ¿Por qué escribir en el estándar de error?

Escribir en el estándar de error puede ser útil para solucionar problemas de depuración en tus programas de Ruby. Con esta técnica, puedes imprimir mensajes de error específicos para evaluar y corregir cualquier problema en tu código.

## Cómo hacerlo

Para escribir en el estándar de error en Ruby, puedes utilizar el método `warn` o el operador `STDERR.puts`.

````Ruby
# Ejemplo utilizando el método `warn`:
warn "¡Este es un mensaje de error!"

# Ejemplo utilizando el operador `STDERR.puts`:
STDERR.puts "¡Este es un mensaje de error!"
````

Ambos métodos imprimirán el mensaje de error en la salida de error estándar, que a menudo se muestra en rojo en tu terminal.

````bash
$ ruby ejemplo.rb
¡Este es un mensaje de error!
````

## Profundizando en el estándar de error

¿Por qué utilizar el estándar de error en lugar de la salida estándar? A veces, tener mensajes de error separados puede hacer que sea más fácil capturar y depurar problemas específicos en el código. Además, es posible que desees filtrar o redirigir la salida estándar para otros propósitos, como escribir en un archivo de log.

Si quieres personalizar aún más tus mensajes de error, también puedes utilizar el método `format` para formatear la salida como un string. De esta manera, puedes incluir información adicional, como la línea de código donde ocurrió el error.

````Ruby
# Ejemplo con el método `format`:
STDERR.puts format("Error en la línea %d: %s", __LINE__, "Este es un mensaje de error.")

# Output:
Error en la línea 6: Este es un mensaje de error.
````

## Ver también

- [Documentación de Ruby sobre el estándar de error](https://ruby-doc.org/core-2.7.1/IO.html#method-c-warn)
- [Artículo sobre cómo utilizar STDOUT y STDERR en Ruby](https://www.baeldung.com/ruby-stdout-stderr)
- [Tutorial sobre depuración de errores en Ruby](https://www.rubyguides.com/2019/06/debugging-ruby/)