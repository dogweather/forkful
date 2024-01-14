---
title:                "Haskell: Extracción de subcadenas"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad esencial en programación, especialmente en Haskell. Permite a los programadores manipular y trabajar con datos de manera más eficiente y efectiva. En este artículo, aprenderemos por qué es importante extraer subcadenas y cómo hacerlo en Haskell.

## Cómo hacerlo

Extraer subcadenas en Haskell es bastante sencillo. Solo necesitas utilizar la función `take` y `drop` para obtener las partes de la cadena que deseas. Por ejemplo, si queremos obtener los primeros 5 caracteres de una cadena, podemos hacerlo de esta manera:

```Haskell
take 5 "Haskell"
```

La salida será `"Hask"`. Del mismo modo, si queremos obtener los últimos 3 caracteres, podemos usar la función `drop` en lugar de `take`:

```Haskell
drop 3 "Haskell"
```

La salida será `"ell"`. También podemos utilizar ambas funciones juntas para extraer una subcadena específica dentro de una cadena. Por ejemplo, si queremos obtener los caracteres del medio de la palabra "Haskell", podemos hacerlo de la siguiente manera:

```Haskell
take 3 (drop 2 "Haskell")
```

La salida será `"ske"`.

También podemos utilizar el operador `!!` para obtener un solo carácter de una cadena, donde el primer carácter tiene el índice 0. Por ejemplo, si queremos obtener el tercer carácter de "Haskell":

```Haskell
"Yaniv" !! 2
```

La salida será `"n"`.

## Profundizando

La función `take` toma dos argumentos como entrada: el número de elementos que quieres extraer y la lista de la que quieres extraerlos. Del mismo modo, la función `drop` también toma dos argumentos: el número de elementos que quieres saltar y la lista de la que quieres saltarlos.

Al usar `take` y `drop` junto con otras funciones y operadores en Haskell, podemos crear diferentes maneras de extraer subcadenas. También podemos utilizar patrones de coincidencia de patrones para extraer subcadenas de manera más precisa.

## Ver también

- [Haskell Wiki - Manipulación de cadenas](https://wiki.haskell.org/Strings)
- [Haskell Docs - Funciones de lista](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html)