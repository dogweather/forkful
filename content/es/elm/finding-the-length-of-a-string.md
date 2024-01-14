---
title:    "Elm: Encontrando la longitud de una cadena"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Si eres nuevo en la programación, encontrar la longitud de una cadena de texto puede parecer una tarea sencilla. Sin embargo, esta habilidad es esencial ya que es una de las funciones más utilizadas en Elm y en la mayoría de los lenguajes de programación. Saber cómo encontrar la longitud de una cadena de texto te permitirá manipular y trabajar con datos de manera más eficiente.

## Cómo hacerlo

En Elm, se puede encontrar la longitud de una cadena de texto utilizando la función `String.length`. Esta función toma una cadena como argumento y devuelve un entero que representa la cantidad de caracteres en esa cadena. Veamos un ejemplo:

```
Elm: String.length "Hola" -- devuelve 4
```

En este ejemplo, la función `String.length` toma la cadena "Hola" como argumento y devuelve el número 4, que es la cantidad de caracteres en la cadena. También podemos usar esta función en conjunto con otras funciones, como `String.slice` para obtener una porción de una cadena y luego encontrar la longitud de esa porción. Por ejemplo:

```
Elm: String.length (String.slice 0 3 "Hola Mundo") -- devuelve 3
```

En este caso, la función `String.slice` devuelve la palabra "Hola" y luego la función `String.length` encuentra la longitud de esa palabra, que es 3.

## Profundizando

Para aquellos que quieren saber más acerca de cómo encontrar la longitud de una cadena de texto en Elm, es importante tener en cuenta que los caracteres unicode se consideran un solo carácter. Esto significa que si tienes una cadena con caracteres como "á" o "ñ", la función `String.length` tomará esto en cuenta y devolverá la cantidad correcta de caracteres.

Además, también se puede encontrar la longitud de una cadena de texto utilizando el método `String.split` para dividir la cadena en una lista y luego encontrar la longitud de esa lista. Otra opción es utilizar la biblioteca `elm-lang/core` que ofrece más funciones útiles para trabajar con cadenas de texto.

## Ver también

- [Documentación de Elm sobre `String.length`](https://package.elm-lang.org/packages/elm-lang/core/latest/String#length)
- [Documentación de Elm sobre `String.split`](https://package.elm-lang.org/packages/elm-lang/core/latest/String#split)
- [Biblioteca `elm-lang/core`](https://package.elm-lang.org/packages/elm-lang/core/latest/)