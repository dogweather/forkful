---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar y reemplazar texto en Elm

## ¿Qué y por qué?
Buscar y reemplazar texto es una tarea común en programación, que implica encontrar una cadena de texto específica y su reemplazo en un texto dado. Esto es útil para la manipulación de datos, y para limpiar o reformatear la salida.

## ¿Cómo hacerlo?
El lenguaje de programación Elm ofrece métodos para realizar estas operaciones. Aquí tienes un ejemplo de cómo puedes buscar y reemplazar texto en Elm:

```Elm
import String

reemplazar:: String -> String -> String -> String
reemplazar antiguo nuevo texto =
    String.replace antiguo nuevo texto

main = 
    let
        textoOriginal = "me gusta programar en Elm"
        textoReemplazado = reemplazar "Elm" "Python" textoOriginal
     in
     text textoReemplazado
```
    
Este código reemplazará la palabra "Elm" con la palabra "Python" en el `textoOriginal`, y el resultado sería:

```
me gusta programar en Python
```
    
## Inmersión profunda
El lenguaje de programación Elm es relativamente nuevo, lanzado en 2012, y no tiene una historia larga relacionada con la búsqueda y reemplazo de texto. Sin embargo, este concepto ha existido en la programación desde los primeros días.

Aunque Elm proporciona el método `String.replace` para buscar y reemplazar texto, también puedes implementar tu propia función utilizando técnicas de programación funcional.

Además, puedes usar la función `String.split` para dividir el texto en una lista de cadenas, luego puedes usar `List.map` para procesar cada cadena, y finalmente `String.join` para unirlas nuevamente en un solo texto.

## Ver también
Para obtener información adicional sobre cómo trabajar con cadenas de texto en Elm, consulta la documentación oficial de la función `String.replace` aquí: [https://package.elm-lang.org/packages/elm/core/latest/String#replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace) 

Además, también puedes encontrar ejemplos útiles y discusión adicional en los foros de la comunidad Elm:
[https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)