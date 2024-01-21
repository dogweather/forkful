---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:21.813562-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La generación de números aleatorios crea secuencias impredecibles de números, esencial en criptografía, simulaciones y hasta en juegos. Los programadores recurrimos a ello para añadir incertidumbre controlada y probar cómo nuestros programas manejan lo inesperado.

## Cómo hacerlo:
Haskell usa el módulo `System.Random` para generar números aleatorios. Primero, importa el módulo. A continuación, te muestro cómo generar un número aleatorio:

```haskell
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let (rndNumber, _) = random gen :: (Int, StdGen)
  print rndNumber
```

Ejecuta esto y obtendrás un número entero aleatorio como salida:

```
1734928476574
```

## Profundizando
En Haskell, el sistema de tipos y la naturaleza funcional de la lengua exigen un tratamiento distinto de la aleatoriedad. Históricamente, la generación de números aleatorios en programación ha evolucionado con los avances en algoritmos y la comprensión de la entropía y los patrones pseudoaleatorios.

Alternativas en Haskell incluyen usar `randomR` para rangos específicos o `getStdRandom` para obtener resultados directamente sin manejar el generador. A nivel de implementación, Haskell usa una semilla (`StdGen`) para comenzar la secuencia aleatoria; cambia la semilla y cambiará la secuencia. Esto es crucial para la reproducibilidad en pruebas o simulaciones.

## Ver También
- Sistema de Tipos de Haskell: https://www.haskell.org/tutorial/types.html
- Documentación de `System.Random`: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
- "Introduction to Randomness and Random Numbers" de random.org para una visión general sobre la aleatoriedad: https://www.random.org/randomness/