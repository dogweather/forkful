---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Haskell: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué
Alguna vez te ha pasado que tienes un texto largo y quieres eliminar todas las letras mayúsculas o todas las comas? O quizás quieres eliminar todas las vocales de un párrafo? En Haskell, existe una forma sencilla de hacerlo utilizando patrones y funciones específicas.

## Cómo hacerlo
Para eliminar caracteres coincidentes con un patrón en Haskell, podemos utilizar la función `filter` junto con la función `not` y el operador `elem`, que verifica si un elemento se encuentra en una lista. Por ejemplo, si queremos eliminar todas las letras mayúsculas de un texto, podríamos hacer lo siguiente:

```Haskell
let texto = "Hola, ESTO es un TEXTO"
let letrasMinusculas = filter (\c -> not(elem c "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) texto
```

Esto nos devolverá el texto sin las letras mayúsculas: "ola, eso es un texto". Como puedes ver, utilizamos una función anónima con el operador `\` para pasar como argumento a `filter` cada letra del texto y luego comprobamos si esa letra se encuentra en la lista de letras mayúsculas. Si es así, la función `not` devolverá falso y esa letra no será incluida en la nueva lista.

También podemos aplicar este método para eliminar otros caracteres, como por ejemplo, todas las comas de un texto:

```Haskell
let texto = "Hola, esto es un texto con muchas comas,,,,,,,,"
let textoSinComas = filter (\c -> not(elem c ",")) texto
```

La nueva lista devolverá: "Hola esto es un texto con muchas comas". Es importante tener en cuenta que el orden de los caracteres en la lista que pasamos a la función `elem` no importa, ya que sólo comprobamos si el carácter está presente o no.

## Profundizando
En Haskell, también podemos utilizar la función `map` para aplicar una función a cada elemento de una lista. Por ejemplo, si queremos eliminar todas las vocales de un texto, podemos hacerlo de la siguiente manera:

```Haskell
let texto = "Esta es una oración con muchas vocales"
let textoSinVocales = map (\c -> if elem c "aeiouAEIOU" then "" else c) texto
```

Esto nos devolverá el texto sin las vocales: "St s n rcón cn mchs vcls". Usamos la misma lógica de `filter`, pero en este caso, en lugar de devolver un booleano en la función anónima, devolvemos el carácter vacío `""` si encontramos una vocal, de lo contrario, devolvemos el propio carácter.

Otra forma de eliminar caracteres coincidentes con un patrón en Haskell es utilizando la función `delete` del módulo `Data.List`. Esta función recibe un elemento y una lista, y elimina la primera ocurrencia de ese elemento en la lista. Por ejemplo:

```Haskell
import Data.List

let numeros = [1,2,3,4,3,2,1]
let numerosSinTres = delete 3 numeros
```

Esto nos devolverá: [1,2,4,2,1]. Como puedes ver, la primera ocurrencia del número 3 fue eliminada de la lista.

## Ver también
- [Documentación de Haskell](https://www.haskell.org/documentation/)
- [Función filter en Haskell](https://www.haskell.org/hoogle/?hoogle=filter)
- [Función map en Haskell](https://www.haskell.org/hoogle/?hoogle=map)
- [Función delete en Haskell](https://www.haskell.org/hoogle/?hoogle=delete)