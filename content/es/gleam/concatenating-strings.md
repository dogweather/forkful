---
title:    "Gleam: Uniendo cadenas de texto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas de texto es una herramienta esencial para cualquier programador. Permite combinar múltiples cadenas en una sola, lo que puede facilitar la manipulación y presentación de datos en una aplicación. En este artículo, aprenderemos cómo hacerlo en el lenguaje de programación Gleam.

## Cómo hacerlo

Para concatenar cadenas de texto en Gleam, podemos utilizar el operador de suma `+` o la función `String.concat()`. Veamos un ejemplo de cada uno:

```Gleam
let saludo = "Hola"
let nombre = "Juan"
let mensaje = saludo + " " + nombre
//resultado: "Hola Juan"

let saludo = "Hola"
let nombre = "Juan"
let mensaje = String.concat(saludo, " ", nombre)
//resultado: "Hola Juan"
```

Como se puede ver en los ejemplos, podemos utilizar el operador de suma para unir directamente las cadenas, o utilizar la función `String.concat()` con cada parte separada por comas. También podemos utilizar variables y valores estáticos en la concatenación.

Otra forma de concatenar cadenas es utilizando la función `String.join()`, que nos permite unir múltiples cadenas en una sola:

```Gleam
let partes = ["Hola", "mundo", "!"]
let mensaje = String.join(" ", partes)
//resultado: "Hola mundo !"
```

## Profundizando

En Gleam, las cadenas de texto son listas de caracteres (`List(char)`). Esto significa que podemos utilizar funciones y métodos de listas en ellas, como `List.append()` y `List.concat()`. Veamos un ejemplo de cómo podemos utilizar esto para concatenar cadenas de forma más dinámica:

```Gleam
fn concat_nombres(nombres: List(string)) {
  List.foldl(
    fn(n, acum) {
      acum ++ " " ++ n
    },
    "Hola",
    nombres
  )
}

concat_nombres(["Juan", "María", "Pedro"])
//resultado: "Hola Juan María Pedro"
```

En este ejemplo, utilizamos la función `List.foldl()` para iterar sobre la lista de nombres y concatenarlos con un espacio en blanco entre cada uno.

## Ver también

- [Documentación oficial de Gleam sobre cadenas de texto](https://gleam.run/book/tour/strings.html)
- [Más información sobre funciones y métodos de listas en Gleam](https://gleam.run/book/std-lib/list.html) 
- [Ejemplos y prácticas con concatenación de cadenas en Gleam](https://github.com/search?q=gleam+string+concat&type=code)