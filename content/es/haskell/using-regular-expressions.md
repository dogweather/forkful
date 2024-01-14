---
title:                "Haskell: Utilizando expresiones regulares"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por qué

Siempre que necesite buscar y manipular patrones de texto de manera eficiente, las expresiones regulares pueden ser una herramienta muy útil y poderosa para los programadores de Haskell.

Las expresiones regulares permiten buscar en una cadena de texto utilizando patrones específicos en lugar de buscar caracteres exactos. Esto es especialmente útil cuando se tienen grandes cantidades de datos o cuando se necesita encontrar patrones complejos dentro de un texto.

Con el uso de expresiones regulares, los programadores pueden escribir algoritmos más eficientes y elegantes para manipular cadenas de texto, ahorrando tiempo y esfuerzo.

## Cómo hacerlo

Para utilizar expresiones regulares en Haskell, es necesario importar el módulo "Text.Regex.Posix". A continuación, se pueden utilizar las funciones "matchRegex" y "matchRegexAll" para buscar y extraer patrones de texto.

Por ejemplo, si tenemos una cadena de texto que contiene varias direcciones de correo electrónico, podemos utilizar una expresión regular para extraer solo los dominios de esos correos electrónicos.

```Haskell
import Text.Regex.Posix

main :: IO ()
main = do
  let email = "ejemplo1@gmail.com, ejemplo2@hotmail.com, ejemplo3@yahoo.com"
  let regex = "[+-]?[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"
  let domains = matchRegexAll regex email
  print domains
  ```

En este ejemplo, la expresión regular utilizada busca patrones de correo electrónico válidos y utiliza la función "matchRegexAll" para devolver una lista de dominios encontrados en la cadena de texto.

El resultado sería: `[Just "gmail.com", Just "hotmail.com", Just "yahoo.com"]`

## Profundizando

Además de la función "matchRegexAll", el módulo "Text.Regex.Posix" también ofrece otras funciones útiles para trabajar con expresiones regulares en Haskell, como "subRegex" para reemplazar patrones en una cadena de texto y "splitRegex" para dividir una cadena en una lista utilizando un patrón como delimitador.

También es importante mencionar que las expresiones regulares en Haskell siguen la sintaxis del estándar POSIX, por lo que es importante estar familiarizado con las reglas y convenciones para obtener los mejores resultados.

¡Experimente con diferentes patrones y funciones para descubrir la versatilidad y potencia de las expresiones regulares en Haskell!

# Ver también

- [Haskell Docs: Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Tutorial: Expresiones regulares en Haskell](https://www.fpcomplete.com/blog/2017/03/reimplementing-grep-explicit-recursion)