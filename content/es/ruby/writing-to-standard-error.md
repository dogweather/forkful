---
title:                "Escribiendo en el error estándar."
html_title:           "Ruby: Escribiendo en el error estándar."
simple_title:         "Escribiendo en el error estándar."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir al error estándar?

Escribir al error estándar es una herramienta útil en la programación de Ruby, ya que permite enviar mensajes de error y ayudar a detectar y solucionar problemas en el código. También puede ser útil para imprimir información de depuración durante el proceso de desarrollo.

## Cómo hacerlo

Para escribir al error estándar en Ruby, se utiliza el método `STDERR.puts` seguido por el texto que se desea imprimir. Por ejemplo:

```Ruby
STDERR.puts "Error: No se ha podido abrir el archivo."
```

Esto imprimirá el mensaje de error en la consola o terminal.

## Profundizando

El error estándar (STDERR) es una corriente de salida que se utiliza para mostrar mensajes de error en lugar de la salida estándar (STDOUT). Esto permite que los mensajes de error se destaquen y puedan ser fácilmente identificados durante la ejecución del programa.

Además de `STDERR.puts`, también hay otros métodos que se pueden utilizar para escribir al error estándar en Ruby, como `STDERR.print` o `STDERR.write`. Estos métodos son similares al `puts`, pero no agregan un salto de línea al final del texto.

## Ver también

- [Documentación de Ruby sobre STDERR](https://ruby-doc.org/core-2.7.2/IO.html#method-c-new-label-Standard+Streams)
- [Artículo de Medium sobre el uso de STDERR en Ruby](https://medium.com/@felipecsl/using-stderr-in-ruby-71b0c47ad904)
- [Video tutorial sobre manejo de errores en Ruby](https://www.youtube.com/watch?v=u_NU--xjmsg)