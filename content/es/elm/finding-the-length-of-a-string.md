---
title:                "Elm: Encontrando la longitud de una cadena"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En este artículo, vamos a explorar cómo encontrar la longitud de una cadena en Elm. Esta es una habilidad importante en la programación, y aprender cómo hacerlo en Elm nos ayudará a construir aplicaciones más eficientes y robustas.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Elm, podemos usar la función `String.length`. Esta función toma una cadena como argumento y devuelve un número que representa la longitud de la cadena. Veamos un ejemplo:

```Elm
let
  cadena = "Hola, soy un string"
  longitud = String.length cadena
in
  "La longitud de la cadena es " ++ (toString longitud)

-- Salida: "La longitud de la cadena es 20"
```

Aquí, creamos una variable `cadena` que contiene nuestra cadena de ejemplo, y luego utilizamos la función `String.length` para obtener su longitud. La función `toString` se utiliza para convertir el número en una cadena, ya que la concatenación solo funciona con cadenas.

También podemos usar la función `String.length` para verificar si una cadena está vacía o no. Si la longitud es cero, significa que la cadena está vacía. Por ejemplo:

```Elm
let
  cadenaVacia = ""
  cadena = "¡Hola!"
in
  if String.length cadenaVacia == 0 then
    "La cadena está vacía"
  else
    "La cadena no está vacía"

-- Salida: "La cadena está vacía"
```

## Profundizando

Ahora que sabemos cómo usar la función `String.length`, es importante entender cómo esta función funciona detrás de escena. En Elm, las cadenas son tratadas como listas de caracteres, por lo que la función `String.length` en realidad cuenta el número de caracteres en la lista.

Podemos ver esto si usamos la función `String.toList`, que convierte una cadena en una lista de caracteres. Por ejemplo:

```Elm
let
  cadena = "Hola"
  listaCaracteres = String.toList cadena
  longitud = List.length listaCaracteres
in
  "La longitud de la cadena es " ++ (toString longitud)

-- Salida: "La longitud de la cadena es 4"
```

Aquí, primero convertimos la cadena en una lista de caracteres y luego usamos la función `List.length` para obtener la longitud de la lista. Vemos que el resultado es el mismo que si hubiéramos usado la función `String.length`.

## Ver también

- [Documentación oficial de Elm sobre cadenas](https://elm-lang.org/docs/strings)
- [Funciones de cadenas en Elm](https://korban.net/elm/elm-strings/)