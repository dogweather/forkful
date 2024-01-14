---
title:    "Gleam: Uniendo cadenas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica clave en la programación y es especialmente útil en el lenguaje de programación Gleam. Esta habilidad te permitirá unir varias cadenas de texto para crear mensajes más complejos y dinámicos en tu código.

## Cómo hacerlo
La concatenación de cadenas se realiza utilizando el operador `++` en Gleam. Por ejemplo, si queremos unir las cadenas "¡Hola" y "mundo!", escribiremos lo siguiente:

```Gleam
"¡Hola" ++ "mundo!"
```

Esto nos dará como resultado la cadena "¡Hola mundo!".

También podemos usar variables en la concatenación de cadenas. Supongamos que tenemos una variable `nombre` que contiene el valor "Juan", y queremos crear un mensaje de bienvenida. Podemos hacerlo de la siguiente manera:

```Gleam
"¡Hola" ++ nombre ++ ", bienvenido!"
```

El resultado de esto sería la cadena "¡Hola Juan, bienvenido!".

## Profundizando
Es importante tener en cuenta que, en la concatenación de cadenas, es necesario tener en cuenta los tipos de datos. Si tratamos de unir una cadena con un número, obtendremos un error. Por ejemplo:

```Gleam
"El número " ++ 10 ++ " es mi favorito"
```

Esto nos dará un error ya que no se pueden concatenar una cadena con un número. Para solucionarlo, debemos convertir el número en una cadena utilizando la función `to_string`. Nuestro código quedaría así:

```Gleam
"El número " ++ to_string(10) ++ " es mi favorito"
```

Ahora obtendremos como resultado la cadena "El número 10 es mi favorito".

## Ver también
- [Documentación oficial del operador de concatenación de cadenas en Gleam](https://gleam.run/book/stdlib/string.md#concatenation) 
- [Ejemplos prácticos de concatenación de cadenas en Gleam](https://github.com/gleam-lang/gleam_stdlib/blob/master/examples/string/concat.gleam)