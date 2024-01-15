---
title:                "Capitalizando una cadena."
html_title:           "Elm: Capitalizando una cadena."
simple_title:         "Capitalizando una cadena."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Elm?

A veces, en la programación, necesitamos convertir una cadena de texto en mayúsculas o minúsculas para que se muestre de cierta manera en nuestra aplicación o para realizar ciertas comparaciones. En este artículo, aprenderás cómo capitalizar una cadena en Elm utilizando métodos simples y eficientes.

## Cómo hacerlo

Para capitalizar una cadena en Elm, utilizamos la función `String.toUpper` que convierte todos los caracteres de una cadena a mayúsculas.

```Elm
casa = "Mi casa es azul."
String.toUpper casa -- Salida: "MI CASA ES AZUL."
```

Del mismo modo, podemos utilizar la función `String.toLower` para convertir una cadena a minúsculas.

```Elm
lugar = "EN EL PARQUE"
String.toLower lugar -- Salida: "en el parque"
```

También podemos utilizar la función `String.capitalize` para capitalizar solo la primera letra de una cadena.

```Elm
nombre = "marta"
String.capitalize nombre -- Salida: "Marta"
```

## Profundizando

En el fondo, las cadenas en Elm son solo una lista de caracteres, por lo que también podemos capitalizar una cadena utilizando funciones de lista como `List.map` y `String.fromList`.

```Elm
titulo = "esta es una historia de amor"
String.fromList (Titulo |> String.toUpper |> List.map Char.toUpper) -- Salida: "ESTA ES UNA HISTORIA DE AMOR"
```

Además, es importante tener en cuenta que en Elm, las cadenas son inmutables, lo que significa que no se pueden modificar directamente. Por lo tanto, siempre debemos asignar el resultado de una función de cadena a una nueva variable en lugar de intentar modificar la cadena original.

## Ver también

- [Documentación oficial de Elm sobre cadenas](https://elm-lang.org/docs/strings)
- [Introducción al lenguaje de programación Elm](https://medium.com/@yonatandoron/introducci%C3%B3n-al-lenguaje-de-programaci%C3%B3n-elm-a0c926fb9c14)
- [Guía práctica para aprender Elm](https://www.elmlang.tech/)