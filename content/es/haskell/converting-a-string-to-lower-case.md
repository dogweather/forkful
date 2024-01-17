---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Haskell: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

¡Hola a todos los lectores! Seguro que alguna vez has oído a los programadores hablar de convertir una cadena a minúsculas. ¿Pero qué significa realmente eso y por qué lo hacen? Bueno, estoy aquí para explicártelo de manera sencilla y directa.

## ¿Qué & Por qué?
Convertir una cadena a minúsculas es simplemente tomar una cadena de texto y transformar todas sus letras a su equivalente en minúsculas. Los programadores suelen hacer esto para facilitar la comparación de cadenas, ya que así se aseguran de que las letras no distorsionen el resultado. Además, en algunos lenguajes de programación, las mayúsculas y minúsculas pueden ser tratadas de manera diferente, así que convertir a minúsculas puede evitar posibles errores.

## Cómo:
En Haskell, podemos convertir una cadena a minúsculas utilizando la función `toLower` del módulo `Data.Char`. Veamos un ejemplo:

```Haskell
import Data.Char

toLower "HOLA MUNDO" 
```
**Salida:** "hola mundo"

Como se puede ver, todas las letras mayúsculas han sido convertidas a minúsculas. Fácil, ¿verdad?

## Excavando más profundo:
Si te interesa conocer más sobre el tema, déjame contarte un poco sobre su origen. En los primeros días de la informática, cuando los ordenadores tenían una capacidad limitada, se almacenaban las letras en formato ASCII (American Standard Code for Information Interchange). En este formato, las letras minúsculas y mayúsculas estaban separadas por un número conocido como el "bit de caso". Actualmente, con la aparición de Unicode, esto ya no es necesario, pero aún sigue siendo una práctica común.

En cuanto a alternativas, existen otras formas de convertir una cadena a minúsculas, dependiendo del lenguaje de programación que uses. Por ejemplo, en Python podemos utilizar la función `lower()` del objeto `str`:

```Python
"HOLA MUNDO".lower()
```
**Output:** "hola mundo"

Finalmente, en cuanto a la implementación, la función `toLower` de Haskell hace uso del estándar Unicode para realizar la conversión de manera eficiente y correcta.

## Echa un vistazo:
Si quieres profundizar aún más en cómo funciona `toLower`, o si simplemente quieres descubrir más sobre este tema en general, te dejo aquí algunos enlaces útiles:

- [Documentación oficial de la función `toLower`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower)
- [Más información sobre ASCII y Unicode](https://www.codigopedia.com/codigo/ascii-y-unicode-diferencias/) 
- [Comparación de la función `toLower` en diferentes lenguajes de programación](https://rosettacode.org/wiki/Change_letter_case#Haskell)

¡Eso es todo por hoy! Espero que este artículo te haya ayudado a entender mejor la conversión de cadenas a minúsculas en Haskell. ¡Hasta la próxima!