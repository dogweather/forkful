---
title:    "Haskell: Extrayendo subcadenas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has tenido que trabajar con cadenas de texto largas y solo necesitabas una parte específica de ella? Este es el escenario perfecto para usar la función de extracción de subcadenas en Haskell. Puedes utilizar esta función para obtener fácilmente una parte de una cadena según tus necesidades. ¡Sigue leyendo para conocer más sobre cómo hacerlo!

## Como hacerlo
Para extraer subcadenas en Haskell, podemos utilizar la función `take` y `drop`. Toma una cadena y un número como parámetros. El número indica cuántos caracteres quieres extraer, mientras que la cadena es aquella de la que se desea extraer. A continuación, se muestra un ejemplo de cómo usar estas funciones en Haskell: 
```Haskell
cadena = "Hola mundo"
extracto = take 4 cadena
putStrLn extracto
```
Este código imprimirá `Hola`. Como puedes ver, solo hemos tomado los primeros cuatro caracteres de la cadena.

Veamos otro ejemplo, utilizando la función `drop`:
```Haskell
cadena = "Hola mundo"
restante = drop 5 cadena
putStrLn restante
```
Este código imprimirá `mundo`. Hemos eliminado los primeros cinco caracteres y hemos quedado con el resto de la cadena. ¡Es así de simple!

## Profundizando
Las funciones de extracción de subcadenas en Haskell también pueden tomar índices como argumentos en lugar de un número determinado de caracteres. Por ejemplo, podemos utilizar la función `take` con dos índices para extraer una subcadena específica de una cadena. El primer índice indica el inicio de la subcadena y el segundo índice indica el final de la misma. Ejemplo: 
```Haskell
cadena = "Hola mundo"
extracto = take 2 7 cadena
putStrLn extracto
```
Este código imprimirá `ola mun`. Hemos tomado una subcadena que comienza en el índice 2 (la tercera letra de la cadena) y termina en el índice 7 (la séptima letra de la cadena). ¡Puedes jugar con diferentes índices para obtener diferentes subcadenas!

## Ver también
- [Documentación de Haskell sobre `take` y `drop`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:take)
- [Más ejemplos de extracción de subcadenas](https://www.tutorialspoint.com/haskell/haskell_string_operations.htm)