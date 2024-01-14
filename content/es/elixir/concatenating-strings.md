---
title:                "Elixir: Concatenando strings"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica esencial en la programación en Elixir. Permite combinar varias cadenas de texto en una sola, lo que resulta muy útil para crear mensajes o imprimir información en la pantalla. Además, es una habilidad fundamental para trabajar con bases de datos y formatear datos de manera adecuada.

## Cómo hacerlo

La concatenación de cadenas en Elixir se realiza utilizando el operador `<>`. Veamos un ejemplo:

```Elixir
nombre = "Juan"
apellido = "González"
saludo = "¡Hola, " <> nombre <> " " <> apellido <> "!"
IO.puts(saludo)
```
Output:
```
¡Hola, Juan González!
```

También es posible concatenar más de dos cadenas a la vez, simplemente encadenando más operadores `<>`. Por ejemplo:

```Elixir
inicio = "Hoy es"
dia = "lunes"
fin = "y hace sol."
frase = inicio <> " " <> dia <> " " <> fin
IO.puts(frase)
```
Output:
```
Hoy es lunes y hace sol.
```

Aunque el operador `<>` es el más común para la concatenación de cadenas en Elixir, también existe la función `String.concat/2` que realiza la misma operación. Su uso es el siguiente:

```Elixir
mensaje = String.concat(["¡", "Hola", " ", "mundo", "!"])
IO.puts(mensaje)
```
Output:
```
¡Hola mundo!
```

## Profundizando

Es importante mencionar que la concatenación de cadenas en Elixir es muy eficiente. A diferencia de otros lenguajes, no se crea una nueva cadena cada vez que se realiza una concatenación. En su lugar, se utilizan referencias a las cadenas originales para crear una única cadena final. Esto significa que el proceso es más rápido y eficiente en términos de memoria.

Por otro lado, también es posible utilizar la interpolación de cadenas en lugar de la concatenación. Esto permite insertar variables directamente en una cadena utilizando el símbolo `#{}`. Veamos un ejemplo:

```Elixir
fruta = "manzana"
color = "roja"
respuesta = "¿Cuál es tu fruta favorita?"
IO.puts("Mi fruta favorita es la #{fruta} #{color}, ¿qué tal la tuya?")
IO.puts("Por cierto, #{respuesta}")
```
Output:
```
Mi fruta favorita es la manzana roja, ¿qué tal la tuya?
Por cierto, ¿Cuál es tu fruta favorita?
```

## Ver también

- [Documentación oficial de Elixir sobre la concatenación de cadenas](https://elixir-lang.org/getting-started/string-interpolation-and-matching.html#string-interpolation)
- [Elixir School: Strings](https://elixirschool.com/es/lessons/basics/strings/)