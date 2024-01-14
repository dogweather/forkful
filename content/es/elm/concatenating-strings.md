---
title:    "Elm: Uniendo cadenas de texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una habilidad importante en la programación, ya que permite unir múltiples cadenas de texto en una sola, facilitando así la manipulación y procesamiento de datos en aplicaciones.

## Cómo hacerlo

Para concatenar cadenas en Elm, podemos usar el operador `++`, que une dos cadenas de texto juntas en una sola. Por ejemplo:

```Elm
"hola" ++ "mundo" == "hola mundo"
```

También podemos usar la función `String.concat`, que toma una lista de cadenas y las une en una sola. Por ejemplo:

```Elm
String.concat ["¡", "Hola", "mundo", "!"] == "¡Hola mundo!"
```

Otra forma de concatenar cadenas es usando la función `String.join`, que toma una lista de cadenas y las une con un separador especificado. Por ejemplo:

```Elm
String.join "-" ["mercurio", "venus", "tierra"] == "mercurio-venus-tierra"
```

## Profundizando

Además de la concatenación simple de dos cadenas, también podemos usar la interpolación de cadenas para construir cadenas más complejas. Esto nos permite insertar valores de variables o expresiones en una cadena. Por ejemplo:

```Elm
"¡Hola, " ++ nombre ++ "!" == "¡Hola, Carlos!"
```

También podemos usar la función `String.fromInt` para convertir números enteros en cadenas y concatenarlos con otras cadenas. Por ejemplo:

```Elm
"Este es el número " ++ (String.fromInt 42) == "Este es el número 42"
```

## Ver también

- Documentación de Elm sobre concatencación de cadenas: https://package.elm-lang.org/packages/elm/core/latest/String#corncat
- Ejemplos de concatenación de cadenas en Elm: https://guide.elm-lang.org/types/strings.html#concatenation
- Tutoriales de Elm en español: https://www.elm-tutorial.org/es/