---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Ruby: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

Si eres un desarrollador de Ruby, es probable que hayas oído hablar de los argumentos de línea de comandos y su importancia en la programación. Son una forma de pasar información a un programa desde la línea de comandos y son extremadamente útiles en la automatización de tareas o en la configuración de programas. A continuación, te mostramos cómo leer los argumentos de línea de comandos en Ruby.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Ruby, utilizamos la variable global `$ARGV`, que almacena un array con los argumentos ingresados. Por ejemplo, si ejecutamos nuestro programa con `ruby mi_programa.rb hola adiós`, `$ARGV` se convierte en `["hola", "adiós"]`. Podemos utilizar un bucle `each` para iterar a través de los argumentos y realizar acciones en función de ellos.

```Ruby
# Ejemplo de un programa que recibe dos argumentos e imprime un mensaje diferente para cada uno

argumentos = $ARGV

argumentos.each do |arg|
  if arg == "hola"
    puts "¡Hola! ¿Cómo estás?"
  elsif arg == "adiós"
    puts "¡Hasta luego! ¡Que tengas un buen día!"
  end
end
```
La salida de este programa sería:

```
¡Hola! ¿Cómo estás?
¡Hasta luego! ¡Que tengas un buen día!
```

## Profundizando

Además de `$ARGV`, también podemos utilizar la gema `optparse` para manejar los argumentos de línea de comandos en una forma más flexible y robusta. Esta gema nos permite definir opciones y argumentos específicos y también agregar mensajes de ayuda para que los usuarios puedan entender mejor cómo ejecutar nuestro programa. Puedes aprender más sobre `optparse` en su [documentación oficial](https://ruby-doc.org/stdlib-2.7.2/libdoc/optparse/rdoc/OptionParser.html).

## Ver también

- [Ruby's ARGV documentation](https://ruby-doc.org/core-2.7.2/ARGF.html)
- [Getting Started with OptParse](https://www.rubyguides.com/2018/08/ruby-optparse/)