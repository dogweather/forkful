---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La búsqueda y reemplazo de texto es el proceso de encontrar cadenas de texto específicas y sustituirlas por otras. Los programadores lo hacen para modificar o corregir el código, mejorar la eficiencia, o incluso cambiar el comportamiento de los programas.

## Cómo se hace:

En Haskell, puedes usar la función `subRegex` del módulo `Text.Regex` para buscar y reemplazar texto. Para utilizarlo, debes importar el módulo así:

```Haskell
import Text.Regex (subRegex, mkRegex)
```

Creemos una función que busque todas las instancias de "buscar" y las reemplace con "reemplazar":

```Haskell
reemplazarTexto :: String -> String -> String -> String
reemplazarTexto buscar reemplazar texto = 
    subRegex (mkRegex buscar) texto reemplazar
```

Prueba la función con esta línea de código:

```Haskell
print (reemplazarTexto "gato" "perro" "El gato está en la caja")
```

Obtendrás:

```Haskell
"El perro está en la caja"
```

## Más a fondo:

La búsqueda y reemplazo de texto es una técnica que se ha utilizado desde los primeros días de la programación. Sin embargo, no todas las implementaciones son iguales. Algunas buscan y reemplazan texto utilizando algoritmos muy eficientes, mientras que otras, como el módulo `Text.Regex` de Haskell, utilizan expresiones regulares, que son mucho más flexibles y poderosas.

Haskell también ofrece la función `replace` del modulo `Data.Text` como una alternativa a `subRegex`. Esta opción es más rápida, pero menos versátil, ya que no soporta expresiones regulares.

Finalmente, está bueno conocer cómo funciona el reemplazo de texto en Haskell detrás de escena. Primero, se compila la expresión regular en un autómata finito determinista. Luego, se itera sobre el texto de entrada y se genera el texto de salida.

## Ver También:

Para más información sobre búsqueda y reemplazo de texto en Haskell, consulta los siguientes recursos:

- Regex Basics en Haskell: www.haskell.org/tutorial/regex.html
- Documentación del módulo Text.Regex: https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html
- Documentación del módulo Data.Text: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html