---
title:    "Haskell: Buscando y reemplazando texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

La búsqueda y reemplazo de texto es una habilidad fundamental en la programación, especialmente en lenguajes funcionales como Haskell. Con esta habilidad, puedes modificar grandes cantidades de texto con rapidez y precisión, ahorrándote tiempo y esfuerzo manual. Además, es una técnica útil para realizar cambios en proyectos más grandes y complejos.

## Cómo hacerlo

Para realizar una búsqueda y reemplazo de texto en Haskell, necesitarás importar el módulo `Text.Regex.Posix` y definir una expresión regular. A continuación, puedes utilizar la función `subRegex` para realizar el reemplazo en una cadena de texto:

```Haskell
import Text.Regex.Posix

-- Definir la expresión regular y el patrón a reemplazar
regex :: String
regex = "\\d+" -- Encuentra cualquier número en la cadena de texto

-- Realizar el reemplazo
subRegex regex "¡Hola 123 mundo!" "adiós" -- Salida: "¡Hola adiós mundo!"
```

También puedes usar la función `subRegexAll` para reemplazar todas las ocurrencias en una cadena de texto:

```Haskell
subRegexAll regex "123 ejemplo de 456 texto" "testing" -- Salida: "testing ejemplo de testing texto"
```

## Inmersión profunda

En Haskell, hay varias funciones disponibles para trabajar con expresiones regulares y realizar búsqueda y reemplazo de texto. Algunas de estas funciones incluyen `matchRegex`, `matchRegexAll` y `getAllMatches`, que permiten obtener información más detallada sobre las coincidencias encontradas en una cadena de texto.

Además, el módulo `Text.Regex.Posix.ByteString` contiene versiones de estas funciones para trabajar con cadenas de bytes en lugar de cadenas de texto, lo que puede ser útil para proyectos que requieren una mayor eficiencia en el manejo de grandes cantidades de datos.

En conclusión, la búsqueda y reemplazo de texto en Haskell puede parecer abrumadora al principio, pero una vez que entiendas cómo funciona y qué funciones están disponibles, puede ser una herramienta muy poderosa y útil en tu caja de herramientas de programación.

## Ver también

- [Documentación de Haskell sobre expresiones regulares](https://www.haskell.org/onlinereport/standard-prelude.html#regexps)
- [Tutorial de búsqueda y reemplazo de texto en Haskell](https://wiki.haskell.org/Haskell_Regular_expressions)
- [Ejemplos de búsqueda y reemplazo en Haskell en Rosetta Code](https://rosettacode.org/wiki/Search_and_replace)