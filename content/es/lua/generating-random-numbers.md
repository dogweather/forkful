---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

La generación de números aleatorios es el proceso de producir números que no siguen ningún patrón predecible. Los programadores generan números aleatorios para crear simulaciones, juegos, pruebas de stress, entre otros.

## Cómo hacerlo:

Puedes generar un número aleatorio en Lua utilizando la función `math.random`. Mira estos ejemplos de código:

```Lua
-- Generar un número aleatorio
print(math.random())  -- salida: un número entre 0 y 1

-- Generar un número entero aleatorio entre 1 y 10
print(math.random(10)) -- salida: un número entre 1 y 10

-- Generar un número entero aleatorio entre 20 y 50
print(math.random(20, 50)) -- salida: un número entre 20 y 50
```
¡Recuerda, cada vez que ejecutes este código, los números resultantes serán diferentes!

## A Fondo:

En la mayoría de los lenguajes de programación modernos existe un enfoque para generar números aleatorios. En Lua, el uso de la función `math.random` sigue el método general de una "semilla" y una "función de aleatoriedad".

* **Contexto histórico**: Originalmente, la generación de números aleatorios se hacía físicamente (como lanzar un dado). Pero con el desarrollo de las computadoras, se crearon métodos algorítmicos.  

* **Alternativas**: Otros idiomas utilizan nombres y métodos diferentes. Por ejemplo, Python tiene `random.randint()`, y JavaScript tiene `Math.random()`. 

* **Detalles de implementación**: Antes de generar números aleatorios con Lua, es importante sembrar el generador con `math.randomseed(os.time())`. Si no, los mismos "números aleatorios" serán generados cada vez que ejecute el programa.

```Lua
math.randomseed(os.time())
print(math.random(10)) -- salida: un número diferente cada vez
```
## Ver Además:

1. [Documentación oficial de Lua para la función Math.random](https://www.lua.org/manual/5.3/manual.html#6.7)
2. [Curso en línea de Programación en Lua](https://www.learn-lua.org/en/Random_Numbers)
3. [Wikipedia: Generador de números aleatorios](https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios)