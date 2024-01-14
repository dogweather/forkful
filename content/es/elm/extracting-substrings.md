---
title:    "Elm: Extracción de subcadenas"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad útil en Elm que te permite manipular cadenas de texto de manera más precisa y eficiente. Esto es especialmente útil en aplicaciones web donde a menudo se necesita manipular y trabajar con grandes cantidades de texto.

## Cómo hacerlo

Para extraer una subcadena de una cadena más grande en Elm, podemos usar la función `slice` que se encuentra en el modulo `String`.

```Elm
import String exposing (slice)

nombre = "Juan Pérez"

primerNombre = slice 0 4 nombre

--output: "Juan"
```

En este ejemplo, estamos extrayendo los primeros cuatro caracteres de la cadena `nombre` que contiene el nombre completo "Juan Pérez". Al usar `slice`, indicamos el índice inicial y el índice final de la subcadena que queremos extraer. En este caso, queremos los primeros cuatro caracteres, por lo que usamos 0 como índice inicial y 4 como índice final.

También podemos usar valores negativos para indicar la posición final contando desde el final de la cadena. Por ejemplo, si queremos obtener los últimos cuatro caracteres del `nombre`, podemos escribir:

```Elm
ultimoNombre = slice -4 0 nombre

--output: "Pérez"
```

Además, podemos usar `slice` para obtener una subcadena de un índice en específico hasta el final de la cadena:

```Elm
apellido = slice 5 (-1) nombre

--output: "Pérez"
```

## Profundizando

La función `slice` en Elm es una forma eficiente de trabajar con cadenas de texto más grandes. Es importante tener en cuenta que `slice` retorna una nueva cadena en lugar de modificar la cadena original. Esto significa que podemos guardar el resultado de `slice` en una nueva variable para usarla más adelante sin afectar la cadena original.

Otra función útil relacionada con el manejo de subcadenas en Elm es `split`. Esta función nos permite separar una cadena en una lista de subcadenas usando un delimitador específico. Por ejemplo:

```Elm
frase = "Bienvenido a mi blog"

palabras = split " " frase

--output: ["Bienvenido", "a", "mi", "blog"]
```

En este caso, usamos el espacio (" ") como delimitador para separar las palabras de la frase en una lista.

## Ver también

- Documentación oficial de la función `slice` en Elm: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Documentación oficial de la función `split` en Elm: https://package.elm-lang.org/packages/elm/core/latest/String#split