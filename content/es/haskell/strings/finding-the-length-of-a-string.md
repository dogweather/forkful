---
date: 2024-01-20 17:47:36.641919-07:00
description: "C\xF3mo hacerlo: En Haskell, usamos la funci\xF3n `length` para obtener\
  \ la longitud de una cadena. Aqu\xED tienes un ejemplo sencillo."
lastmod: '2024-03-13T22:44:59.110412-06:00'
model: gpt-4-1106-preview
summary: "En Haskell, usamos la funci\xF3n `length` para obtener la longitud de una\
  \ cadena."
title: Calculando la longitud de una cadena
weight: 7
---

## Cómo hacerlo:
En Haskell, usamos la función `length` para obtener la longitud de una cadena. Aquí tienes un ejemplo sencillo:

```haskell
longitudCadena :: String -> Int
longitudCadena s = length s

main :: IO ()
main = print (longitudCadena "¡Hola, mundo!")
```

Salida esperada:

```
13
```

## Análisis Detallado
Históricamente, la función `length` ha sido parte del estándar de Haskell, accesible a través del módulo `Prelude`. Es directa, pero hay consejos a considerar. 

Primero, `length` es O(n), lo que significa que su tiempo de ejecución está en proporción al tamaño de la lista (o cadena, que es una lista de caracteres). Es una sencillez costosa para cadenas largas.

Alternativas incluyen utilizar `foldr` para evitar crear listas intermedias o `Data.Text.length` si estás trabajando con el tipo `Text` que es más eficiente para los datos de texto grandes.

En cuanto a la implementación, `length` recorre toda la lista, contando los elementos. No es recomendable para listas infinitas o muy largas, por razones obvias.

## Ver También
- Haskell Prelude Documentation sobre `length`: [Haskell Prelude - length](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:length)
- Optimización con `Data.Text`: [Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- Uso de `foldr`: [Haskell Foldr](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:foldr)
