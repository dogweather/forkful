---
title:                "Calculando la longitud de una cadena"
aliases:
- /es/elm/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:15.502432-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
En elm, encontrar la longitud de una cadena significa contar cuántos caracteres contiene. Los programadores lo hacen para validar entradas, limitar texto en la interfaz de usuario, o simplemente para manejar datos de manera más precisa.

## Cómo hacerlo:
Para obtener la longitud de una cadena en Elm, utiliza la función `String.length`. Aquí tienes un ejemplo:

```Elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "¡Hola Mundo!"))
-- Salida: "12"
```
El código usa `String.fromInt` para convertir el resultado de `String.length` en una cadena y poder mostrarlo.

## Inmersión Profunda
Históricamente, la función `String.length` es un concepto común en muchos lenguajes de programación, pero en Elm se maneja de una manera segura y directa debido a su naturaleza funcional y a su sistema de tipos. No hay alternativas integradas en Elm para `String.length` porque la función es clara y eficiente. Elm cuenta los caracteres teniendo en cuenta que, desde Elm 0.19, las cadenas son siempre UTF-8, por lo que `String.length` devuelve el número correcto de puntos de código Unicode en la cadena.

## Ver También
- La documentación oficial de Elm para [cadenas de texto (Strings)](https://package.elm-lang.org/packages/elm/core/latest/String)
