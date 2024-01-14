---
title:    "Elm: Concatenando cadenas"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué
La concatenación de cadenas es una técnica útil en programación Elm que te permite combinar varias cadenas de texto en una sola. Esto puede ser útil para crear mensajes dinámicos, etiquetas personalizadas o incluso para formatear datos.

## Cómo hacerlo

La concatenación de cadenas se puede lograr utilizando el operador "+" o la función `String.concat` en Elm. Veamos algunos ejemplos prácticos:

```elm
nombreCompleto = "Juan" ++ " " ++ "Pérez"
```
En este caso, el resultado de `nombreCompleto` sería "Juan Pérez".

```elm
saludo = "¡Hola" ++ " " ++ nombreCompleto ++ "!"
```

Aquí, el resultado de `saludo` sería "¡Hola Juan Pérez!".

También es posible concatenar más de dos cadenas utilizando la función `String.concat`, que toma una lista de cadenas como argumento. Veamos un ejemplo:

```elm
colores = ["rojo", "verde", "azul"]
coloresFavoritos = String.concat ["Mis colores favoritos son", String.join ", " colores, "."]
```
En este caso, el resultado de `coloresFavoritos` sería "Mis colores favoritos son rojo, verde, azul."

## Profundizando
La concatenación de cadenas puede parecer una técnica sencilla, pero es importante entender cómo funciona en Elm. Cuando se utiliza el operador "+", se crea una nueva cadena a partir de las cadenas originales. Esto puede ser costoso en términos de rendimiento y uso de memoria cuando se trata de cadenas largas o de muchas concatenaciones.

Por otro lado, la función `String.concat` es más eficiente en cuanto al rendimiento, ya que utiliza una técnica conocida como "construcción incremental" para concatenar las cadenas. Esto significa que la función agrega cada nueva cadena a la existente en lugar de crear una nueva cadena cada vez. Sin embargo, es importante tener en cuenta que esta función sólo funciona con listas de cadenas y no con cadenas individuales.

En resumen, la concatenación de cadenas es una técnica útil en programación Elm, pero es importante considerar el rendimiento cuando se trabaja con cadenas largas o muchas concatenaciones.

## Ver también
- [Documentación oficial de Elm sobre la concatenación de cadenas](https://guide.elm-lang.org/strings/concatenation.html)
- [Ejemplos prácticos de concatenación de cadenas en Elm](https://elmprogramming.com/concatenate-strings-elm.html)
- [Otras técnicas de manipulación de cadenas en Elm](https://medium.com/@Dobiasd/string-manipulation-in-elm-532319a0a281)