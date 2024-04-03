---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:48.395236-07:00
description: "Capitalizar una cadena implica transformar el car\xE1cter inicial de\
  \ una cadena dada a may\xFAsculas mientras se mantienen el resto en min\xFAsculas,\
  \ a menudo por\u2026"
lastmod: '2024-03-13T22:44:58.962766-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena implica transformar el car\xE1cter inicial de una\
  \ cadena dada a may\xFAsculas mientras se mantienen el resto en min\xFAsculas, a\
  \ menudo por motivos de formato estandarizado o legibilidad."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
En Elm, no hay una función integrada específicamente para capitalizar cadenas. Sin embargo, puedes lograr esto fácilmente utilizando funciones del módulo `String` incluido, como `toUpper`, `toLower`, `left` y `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Ejemplo de uso
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Salida: "Hello World"
```

Para escenarios más complejos o si prefieres usar una biblioteca que ofrezca una manera directa de capitalizar cadenas, podrías considerar un paquete de terceros como `elm-community/string-extra`. Sin embargo, hasta mi última actualización, el ecosistema de Elm alienta a tratar tales tareas usando funciones integradas para mantener el lenguaje y los proyectos esbeltos.

```elm
import String.Extra as StringExtra

-- En caso de que haya una función `capitalize` en una biblioteca de terceros
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Ejemplo de uso con función hipotética de biblioteca
main =
    "this is elm" |> capitalizeWithLibrary
    -- Salida hipotética: "This is elm"
```

Siempre verifica el repositorio de paquetes de Elm para las últimas y más preferidas bibliotecas para la manipulación de cadenas si estás buscando funcionalidades adicionales más allá de la biblioteca estándar.
