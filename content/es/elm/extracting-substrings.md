---
title:                "Extrayendo subcadenas"
html_title:           "Elm: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

¡Hola programadores de Elm! ¿Alguna vez has necesitado extraer partes de texto de una cadena más grande? Puede que lo hayas hecho sin darte cuenta, como al buscar una palabra en un documento o al formatear una fecha. De eso se trata extraer subcadenas: obtener una parte de una cadena más grande que cumpla con ciertos requisitos. A continuación, te mostraremos cómo hacerlo en Elm y por qué es importante para los programadores.

## ¿Qué & Por qué?
Extraer subcadenas es una técnica común en la programación que implica seleccionar una parte de una cadena de texto más grande. Los programadores lo hacen para manipular y utilizar la información de manera más eficiente. Por ejemplo, si tenemos una cadena de texto con una dirección de correo electrónico, podemos extraer solo el nombre de usuario para usarlo en otra parte de nuestro programa.

## Cómo hacerlo
En Elm, podemos usar la función ```String.slice``` para extraer una subcadena específica. Por ejemplo, supongamos que tenemos una cadena con el siguiente formato: "Nombre Apellido, Edad". Podemos extraer solo el nombre de la siguiente manera:

```
Elm String.slice "Nombre Apellido, Edad" 0 6
```

Esto nos dará como resultado la subcadena "Nombre", ya que 0 es el índice del primer carácter y 6 es el índice del carácter inmediatamente después del nombre.

Otra forma útil de extraer subcadenas es a través de expresiones regulares. Estas expresiones nos permiten buscar patrones específicos en una cadena de texto y extraer la información que cumpla con esos patrones. Podemos utilizar la función ```Regex.find``` para esto. Por ejemplo, si queremos extraer solo la palabra "Elm" de una cadena que contiene "Hola a todos, ¡bienvenidos a Elm!", podemos hacer lo siguiente:

```
Elm Regex.find (Regex.fromString "([a-z]+),") "Hola a todos, ¡bienvenidos a Elm!"
```
Esto nos devolverá un resultado con la palabra "Elm".

## Profundizando
La extracción de subcadenas se ha utilizado durante mucho tiempo en la programación, incluso en lenguajes antiguos como C y Java. Sin embargo, en Elm es mucho más fácil y seguro gracias a la inferencia de tipos y la inmutabilidad de los datos.

Alternativamente a la función ```String.slice```, también podemos usar ```String.left``` y ```String.right``` para extraer una cantidad específica de caracteres desde el inicio o el final de una cadena.

Es importante tener en cuenta que al extraer subcadenas, es posible que obtengamos resultados inesperados si no tenemos en cuenta los índices y los patrones cuidadosamente. Por eso, siempre es recomendable probar y depurar nuestro código.

## Ver también
Si quieres aprender más sobre manipulación de cadenas y otras técnicas útiles en Elm, te recomendamos revisar la documentación oficial en https://guide.elm-lang.org. También puedes explorar la librería de palabras clave de Elm en https://package.elm-lang.org/. ¡Diviértete explorando!