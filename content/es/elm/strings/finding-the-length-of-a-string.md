---
date: 2024-01-20 17:47:15.502432-07:00
description: "C\xF3mo hacerlo: Para obtener la longitud de una cadena en Elm, utiliza\
  \ la funci\xF3n `String.length`. Aqu\xED tienes un ejemplo."
lastmod: '2024-03-13T22:44:58.970468-06:00'
model: gpt-4-1106-preview
summary: "Para obtener la longitud de una cadena en Elm, utiliza la funci\xF3n `String.length`."
title: Calculando la longitud de una cadena
weight: 7
---

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
