---
title:                "Lectura de argumentos de línea de comandos"
html_title:           "Elm: Lectura de argumentos de línea de comandos"
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¡Lee y entiende mejor tus argumentos de línea de comandos en Elm!

## ¿Qué y por qué?

Leer los argumentos de línea de comandos es una parte fundamental en la programación. Es una manera de obtener información ingresada por el usuario al ejecutar un programa. Los programadores utilizan esta información para personalizar el funcionamiento de sus programas y hacerlos más eficientes para el usuario.

## ¡Cómo hacerlo!

En Elm, podemos acceder a los argumentos de línea de comandos a través de la función ```Cmd.capture```. Esta función toma una función y la ejecuta con los argumentos de línea de comandos como parámetro.

```
Elm . La funcion.Cmd.capture `` `tambien nos permite controlar el formato de los argumentos al convertirlos en una lista de cadenas con la funcion ```Args.arguments```.

El siguiente ejemplo convierte los argumentos en una lista de cadenas y los imprime en la consola:

```
Elm . 
import Args
import Html

main =
  Html.text (toString (Args.arguments))
```

Ejecutando el programa con los siguientes comandos:

```
elm - ArgExample hello world
```

Nos dará como resultado:

```
["hello", "world"]
```

## Profundizando

La lectura de argumentos de línea de comandos es una técnica común en muchos lenguajes de programación, ya que permite al usuario interactuar con el programa de manera más dinámica. Antes de la invención de la interfaz de usuario, los argumentos de línea de comandos eran la única forma de controlar un programa.

Si prefieres una forma más sencilla de interactuar con el usuario, puedes utilizar una biblioteca de interfaz de usuario como [elm-runtime-editor](https://github.com/elm-lang/elm-runtime-editor). Sin embargo, la lectura de argumentos de línea de comandos sigue siendo útil para ciertos casos, como la ejecución de scripts o la personalización de programas de línea de comandos.

En términos de implementación, Elm maneja los argumentos de línea de comandos a través de una interfaz de bajo nivel con JavaScript para acceder al objeto "process" del navegador.

## Ver también

- [Documentación oficial de Elm sobre Cmd.capture](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#capture)
- [Ejemplo de cómo leer argumentos de línea de comandos en Elm](https://ellie-app.com/cnRbnnb578ra1)
- [Librería para interfaces de usuario en Elm](https://github.com/elm-lang/elm-runtime-editor)