---
title:                "Verificando si existe un directorio"
html_title:           "Elixir: Verificando si existe un directorio"
simple_title:         "Verificando si existe un directorio"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación, a veces necesitamos verificar si un directorio existe antes de proceder con una acción, como la escritura de un archivo. Esto se hace para evitar errores de tiempo de ejecución y problemas relacionados con la operación inexistente o la mala manipulación de directorios.

## ¿Cómo hacerlo?

En Elixir, podemos usar la función `File.dir?/1` para verificar la existencia de un directorio. Aquí te dejo un ejemplo:

```Elixir
defmodule Ejemplo do
  def verificar_directorio(ruta) do
    if File.dir?(ruta) do
      IO.puts "El directorio existe"
    else
      IO.puts "El directorio no existe"
    end
  end
end

Ejemplo.verificar_directorio("/home/usuario/directorio")
```

Este programa imprimirá "El directorio existe" si el directorio dado existe, y "El directorio no existe" si no es así.

## Inmersión profunda

Elixir es un lenguaje de programación funcional moderno donde el módulo `File` proporciona funciones para trabajar con archivos y directorios. `File.dir?/1` es una de esas funciones. Esta función simplemente verifica si el directorio en la ruta dada existe y devuelve `true` si es así, y `false` en caso contrario.

En el pasado, con los lenguajes que no ofrecían tales funciones incorporadas, los programadores tenían que implementar su propia lógica para verificar la existencia de un directorio, a menudo invocando comandos del sistema operativo.

Como alternativa, también puedes usar la función `File.ls/1` que te proporciona una lista de archivos en el directorio que especificaste. Puedes usar esto para verificar la existencia de un directorio, pero si solo necesitas verificar la existencia, `File.dir?/1` es una opción más eficiente y clara.

## Ver también

Para una comprensión más profunda del módulo `File` y otras funciones relacionadas, te recomiendo que consultes la documentación oficial en: https://hexdocs.pm/elixir/File.html

Además, si necesitas cavar más profundo en el lenguaje Elixir en general, el libro "Learn Functional Programming with Elixir" de Ulisses Almeida es excelente: https://pragprog.com/titles/cdc-elixir/learn-functional-programming-with-elixir/