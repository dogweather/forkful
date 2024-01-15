---
title:                "Utilizando expresiones regulares"
html_title:           "Haskell: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Porqué
Es posible que alguna vez hayas escuchado acerca de las expresiones regulares y te hayas preguntado para qué se usan. En resumen, las expresiones regulares son patrones que se utilizan para buscar y manipular cadenas de texto de manera eficiente. Son muy útiles en tareas de procesamiento de texto, como la validación de formularios, búsqueda de cadenas específicas o reemplazo de texto.

## Cómo Hacerlo
Haskell tiene un módulo incorporado llamado `Text.Regex` que nos permite utilizar expresiones regulares. Para comenzar, primero debemos importar este módulo en nuestro código.

```Haskell
import Text.Regex
```

Una vez importado el módulo, podemos utilizar la función `matchRegex` para buscar una cadena que coincida con nuestro patrón. Por ejemplo, si queremos verificar si una cadena contiene una dirección de correo electrónico válida, podemos usar el patrón `^[A-Za-z0-9]+@[A-Za-z]+\.[A-Za-z]+$`.

```Haskell
let emailRegex = "^[A-Za-z0-9]+@[A-Za-z]+\.[A-Za-z]+$"
matchRegex emailRegex "ejemplo@ejemplo.com"
-- Output: Just "ejemplo@ejemplo.com"
```

La función `matchRegex` devuelve una lista de todos los resultados que coinciden con el patrón, pero en caso de no encontrar coincidencias, devuelve `Nothing`.

## Profundizando
Hay muchos más patrones y funciones que podemos utilizar en expresiones regulares en Haskell. Por ejemplo, la función `subRegex` nos permite reemplazar una cadena específica en una cadena de texto utilizando un patrón. Podemos utilizarlo para "enmascarar" un número de tarjeta de crédito en un texto:

```Haskell
let cardNumberRegex = "[0-9]{4}[- ][0-9]{4}[- ][0-9]{4}[- ][0-9]{4}"
subRegex cardNumberRegex "La tarjeta de crédito es 1234-1234-1234-1234" "****-****-****-****"
-- Output: "La tarjeta de crédito es ****-****-****-****"
```

Para aprender más sobre expresiones regulares en Haskell, puedes consultar la documentación oficial del módulo `Text.Regex` o explorar ejemplos en línea. ¡Practica y sigue aprendiendo!

## Ver También
- [Documentación de Text.Regex en Haskell](https://hackage.haskell.org/package/regex)
- [Ejemplos de expresiones regulares en Haskell](https://wiki.haskell.org/Regular_expressions)