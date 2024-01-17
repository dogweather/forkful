---
title:                "Concatenando cadenas"
html_title:           "Elm: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Concatenar cadenas es básicamente unir dos o más cadenas de texto en una sola. Los programadores lo hacen para evitar que el código sea repetitivo y para construir mensajes personalizados para el usuario.

## Cómo:
En Elm, esto se puede lograr de diferentes maneras dependiendo de la situación. A continuación, se presentan algunos ejemplos y su respectiva salida:

```
Elm.concat ["¡Hola, ", "mundo!"] 
-- Salida: "¡Hola, mundo!" 

Elm.append "Elm " "es genial" 
-- Salida: "Elm es genial" 

Elm.concat ["Elm", " " , "es", " ", "genial"] 
-- Salida: "Elm es genial"
```

## Profundizando:
La concatenación de cadenas se ha utilizado desde los inicios de la programación. Es una técnica común en muchos lenguajes de programación, incluyendo Elm. Alternativas a la concatenación en Elm incluyen el uso de string interpolation y el uso de listas de caracteres.

## Ver también:
- [Documentación oficial de Elm sobre strings](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Ejemplos de concatenación en Elm](https://github.com/fpangburn/Elm-String-Examples)