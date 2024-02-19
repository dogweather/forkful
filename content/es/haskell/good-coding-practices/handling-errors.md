---
aliases:
- /es/haskell/handling-errors/
date: 2024-01-26 00:52:54.127365-07:00
description: "Manejar errores en la programaci\xF3n es gestionar lo inesperado\u2014\
  cosas que pueden salir mal. Los programadores lo hacen para asegurar que sus programas\u2026"
lastmod: 2024-02-18 23:09:10.035932
model: gpt-4-1106-preview
summary: "Manejar errores en la programaci\xF3n es gestionar lo inesperado\u2014cosas\
  \ que pueden salir mal. Los programadores lo hacen para asegurar que sus programas\u2026"
title: Manejo de errores
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Manejar errores en la programación es gestionar lo inesperado—cosas que pueden salir mal. Los programadores lo hacen para asegurar que sus programas puedan afrontar estas situaciones con gracia, sin colapsar o producir resultados incorrectos.

## Cómo hacerlo:
Haskell maneja errores de manera sólida a través de tipos como `Maybe` y `Either`. Aquí hay un vistazo rápido:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Dividir por cero no está permitido, así que retornamos Nothing.
safeDivide x y = Just (x `div` y)  -- De lo contrario, estamos bien, devolvemos el resultado en un Just.

-- Veámoslo en acción:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Para un manejo de errores más complejo, entra en juego `Either`:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Error de división por cero."  -- Esta vez, el error lleva un mensaje.
safeDivideEither x y = Right (x `div` y)

-- Y en uso:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Error de división por cero."
```

## Inmersión Profunda
En el mundo de Haskell, el manejo de errores tiene una larga historia. Antiguamente, los errores podían derribar todo tu programa—nada divertido. El sistema de tipos de Haskell ofrece caminos para hacer esto mucho menos probable. Tenemos `Maybe` y `Either`, pero hay otros como `Exceptions` y `IO` para diferentes escenarios.

`Maybe` es simple: obtienes `Just` algo si todo va bien, o `Nothing` si no es así. `Either` lo lleva al siguiente nivel, permitiéndote devolver un mensaje de error (`Left`) o un resultado exitoso (`Right`).

Ambos son puros, lo que significa que no alteran el mundo exterior, un gran tema en Haskell. Evitamos las trampas de excepciones no comprobadas que aquejan a algunos otros lenguajes.

Para aquellos que no están satisfechos con `Maybe` y `Either`, bibliotecas como `Control.Exception` ofrecen un manejo de errores más tradicional y de estilo imperativo a través de excepciones. Pero usarlas demasiado libremente puede complicar las cosas, por lo que la comunidad a menudo se adhiere a los tipos.

## Ver También
Profundice con:

- La documentación propia de Haskell: [Haskell](https://haskell.org/documentation)
- Ideal para principiantes: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
