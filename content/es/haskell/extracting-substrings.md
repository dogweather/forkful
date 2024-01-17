---
title:                "Extrayendo subcadenas"
html_title:           "Haskell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Extraer subcadenas es una técnica común utilizada por los programadores para obtener una parte específica de una cadena de texto. Esto puede ser útil en situaciones en las que solo se necesita una parte de la información contenida en una cadena más larga. Por ejemplo, al trabajar con datos de una base de datos, a menudo es necesario extraer valores individuales o campos de una cadena que representa una entrada completa.

## Cómo hacerlo:

Para extraer una subcadena en Haskell, se puede utilizar la función `take` que toma dos argumentos: el número de elementos que se desean extraer y la cadena original. Por ejemplo, si queremos extraer los tres primeros caracteres de la cadena "Hola mundo", escribiríamos lo siguiente:

```
take 3 "Hola mundo"
```

Esto devolvería "Hol" como resultado. También se puede utilizar la función `drop` para eliminar una determinada cantidad de elementos al principio de una cadena.

Otra forma de extraer subcadenas es utilizando la función `takeWhile` que toma un predicado y una lista y devuelve una lista con los elementos que cumplen con ese predicado. Por ejemplo, si queremos obtener todos los números pares de una lista de números, podríamos usar lo siguiente:

```
takeWhile even [2, 4, 7, 10, 11, 12]
```

Esto devolvería [2, 4] como resultado.

## Profundizando:

La técnica de extraer subcadenas es comúnmente utilizada en lenguajes de programación y no se limita solo a Haskell. Otros lenguajes, como Java y C++, también tienen funciones y métodos para extraer subcadenas de una cadena.

Además de la función `take`, también se puede utilizar la función `substring` en Haskell para extraer una subcadena. La diferencia es que `substring` toma tres argumentos: la cadena original, el índice de inicio y el índice final. Este enfoque permite una mayor flexibilidad al especificar qué parte exacta de la subcadena deseamos extraer.

Por último, también es importante tener en cuenta que la mayoría de los lenguajes de programación indexan las cadenas a partir de 0, lo que significa que el primer carácter de una cadena tiene el índice 0, el segundo el índice 1, y así sucesivamente.

## Ver también:

- Documentación oficial de Haskell: [https://www.haskell.org/documentation/](https://www.haskell.org/documentation/)
- Artículo sobre extracción de subcadenas en Java: [https://www.geeksforgeeks.org/java-string-substring/](https://www.geeksforgeeks.org/java-string-substring/)
- Página de referencia para la función `take` en Haskell: [https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:take](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:take)