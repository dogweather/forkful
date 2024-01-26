---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:56.021583-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios es el proceso de obtener valores numéricos impredecibles. Los programadores los usan para todo, desde juegos hasta simulaciones y seguridad de datos.

## Cómo Hacerlo:
En Fish Shell, puedes obtener números aleatorios usando el comando `random`. Aquí unos ejemplos sencillos:

```Fish Shell
# Generar un número aleatorio entre 1 y 10
set -l numero_aleatorio (random 1 10)
echo $numero_aleatorio
```

```Fish Shell
# Generar un número aleatorio y guardarlo en una variable
set -l num (random)
echo $num
```

```Fish Shell
# Generar 5 números aleatorios entre 1 y 100
for i in (seq 5)
    echo (random 1 100)
end
```

Muestra de salida para el primer código:
```
7
```

## Profundización
Históricamente, la generación de números aleatorios en computadoras no ha sido verdaderamente "aleatoria" sino pseudoaleatoria, dependiendo de algoritmos específicos. En sistemas operativos basados en Unix, como MacOS y Linux, se suelen usar dispositivos como `/dev/random` y `/dev/urandom` para obtener entropía del sistema y producir resultados más impredecibles.

Alternativas a `random` en otras shells incluyen `$RANDOM` en Bash o el uso de lenguajes de programación específicos que tienen sus propias funciones de números aleatorios, como `rand()` en PHP o `Math.random()` en JavaScript.

En cuanto a la implementación en Fish, `random` es una función incorporada que por defecto genera un número entre 1 y 32767. Puedes especificar un rango para limitar los resultados.

## Ver También
- Documentación oficial de Fish Shell sobre el comando `random`: https://fishshell.com/docs/current/cmds/random.html
- Un artículo sobre la teoría de generación de números pseudoaleatorios: https://www.random.org/randomness/
- Tutorial de Fish Shell para principiantes: https://fishshell.com/docs/current/tutorial.html
