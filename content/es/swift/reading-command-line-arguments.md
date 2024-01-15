---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Swift: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

Leer argumentos de línea de comandos es una habilidad esencial en la programación Swift, ya que permite interactuar con el usuario y adaptar la ejecución del programa en función de los parámetros ingresados. Al entender cómo leer estos argumentos, podrás desarrollar aplicaciones más interactivas y personalizadas para tus usuarios.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Swift, utilizamos la clase `CommandLine`. Primero, debemos asegurarnos de importarla al inicio de nuestro código:

```Swift
import Foundation
```

Luego, podemos acceder a los argumentos ingresados por el usuario usando la propiedad `arguments` de la clase `CommandLine`. Esta propiedad es un array que contiene todos los argumentos como strings, incluyendo el nombre del programa como primer elemento. Por ejemplo, si ejecutamos nuestro programa con el comando `swift myProgram argument1 argument2`, los elementos del array `arguments` serán `[myProgram, argument1, argument2]`.

Podemos obtener el valor de cada argumento accediendo al índice correspondiente en el array. Por ejemplo, para obtener el valor del segundo argumento, usamos `arguments[1]`.

Es importante recordar que los argumentos siempre serán strings, por lo que deberemos convertirlos a los tipos de datos adecuados dependiendo de su uso en nuestro programa.

## Profundizando

Además de leer los argumentos de línea de comandos ingresados por el usuario, también podemos modificarlos durante la ejecución del programa. Por ejemplo, podemos eliminar elementos del array `arguments` usando el método `removeFirst()`. Esto es útil si queremos eliminar el nombre del programa del array y trabajar solamente con los argumentos ingresados por el usuario.

Otra funcionalidad interesante es la posibilidad de leer argumentos opcionales. Para ello, utilizamos la función `option`, que nos permite especificar el nombre, abreviación y descripción de un argumento opcional. Luego, podemos utilizar el método `parse` para verificar si el usuario ingresó ese argumento y obtener su valor.

Puedes encontrar más información sobre estas funcionalidades en la documentación de Swift.

## Ver también
- [Documentación oficial de Swift](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html)