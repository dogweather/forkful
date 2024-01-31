---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Las expresiones regulares (regex) sirven para buscar patrones en texto. Los programadores las usan por su potencia y flexibilidad en tareas de validación, búsqueda y reemplazo de strings.

## Cómo hacerlo:
Elm no tiene soporte incorporado para regex, pero puedes usar [`elm/regex`](https://package.elm-lang.org/packages/elm/regex/latest/). Instálalo y sigue estos ejemplos:

```Elm
import Regex

-- Verificar si un string concuerda con un patrón regex
emailRegex : Regex.Regex
emailRegex = Regex.fromString "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" |> Maybe.withDefault Regex.never

isValidEmail : String -> Bool
isValidEmail email =
    Regex.contains emailRegex email

-- Uso
isValidEmail "usuario@ejemplo.com"  -- devuelve True
isValidEmail "noesemail"            -- devuelve False
```

## Inmersión Profunda
Las expresiones regulares nacieron en los años 50 con teoría de autómatas. Elm las maneja a través del paquete `elm/regex`, que internamente usa JavaScript. Alternativas incluyen funciones de cadenas (`String`) para búsquedas sencillas. Tener cuidado con su uso, ya que son costosas en términos de rendimiento.

## Ver También
- Documentación de `elm/regex`: [Elm Regex Documentation](https://package.elm-lang.org/packages/elm/regex/latest/)
- Tutorial de regex en general: [RegexOne](https://regexone.com/)
- Ejercicios de expresiones regulares: [Regex Crossword](https://regexcrossword.com/)
