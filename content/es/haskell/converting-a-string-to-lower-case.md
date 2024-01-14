---
title:                "Haskell: Convirtiendo una cadena a minúsculas"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
 Muchas veces en la programación, nos encontramos con la necesidad de manipular cadenas de texto y una de las operaciones más comunes es convertirlas a minúsculas. Ya sea para hacer comparaciones de cadenas de igual tamaño, validar datos ingresados por el usuario, o simplemente para fines estéticos. En este blog post, aprenderemos cómo convertir cadenas de texto a minúsculas en Haskell de manera sencilla y eficiente.

## Cómo hacerlo
Para convertir una cadena de texto a minúsculas en Haskell, podemos utilizar la función `toLower` del módulo `Data.Char`. Esta función tiene la siguiente definición:

```Haskell
toLower :: Char -> Char
```

Esto significa que toma un carácter como argumento y devuelve el mismo carácter en minúscula. Podemos hacer uso de la función `map` para aplicar `toLower` a cada carácter de una cadena de texto. Veamos un ejemplo de cómo utilizarlo:

```Haskell
Prelude> import Data.Char
Prelude Data.Char> map toLower "Haskell"
"haskell"
```

Aquí, hemos importado el módulo `Data.Char` y luego utilizado la función `map` para aplicar `toLower` a cada carácter de la cadena "Haskell". El resultado es una nueva cadena en minúsculas: "haskell".

## Profundizando
Ahora que sabemos cómo usar la función `toLower`, es importante entender cómo funciona por debajo. En Haskell, las cadenas de texto son listas de caracteres, por lo que cuando aplicamos `map` a una cadena, estamos aplicando la función a cada elemento de la lista (es decir, cada carácter). Además, en Haskell, los caracteres se representan internamente como números (códigos ASCII), y la función `toLower` simplemente resta 32 a estos códigos para convertirlos a sus equivalentes en minúsculas.

## Ver también
- [Documentación de `Data.Char`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Tutorial de Haskell en español](https://www.haskell.org/documentation.es.html)
- [Artículo sobre el uso de `map` en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#map)