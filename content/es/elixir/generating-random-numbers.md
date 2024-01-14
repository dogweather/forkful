---
title:                "Elixir: Generación de números aleatorios"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Las Generación de números aleatorios es una habilidad importante en la programación de Elixir. Puede ser utilizada para juegos, experimentos, pruebas y otras aplicaciones donde se requiere una entrada aleatoria.

## Cómo hacerlo
```Elixir
# Generar un número entero aleatorio entre 1 y 10
Random.rand(1..10) 
#=> 7

# Generar un número de punto flotante aleatorio entre 0 y 1
Random.float 
#=> 0.45322

# Generar un número de punto flotante aleatorio entre 10 y 100
Random.float(10..100) 
#=> 84.6576
```

## Profundizando
La función `Random.rand/1` toma un rango como argumento y genera un número entero aleatorio dentro de ese rango. Mientras tanto, la función `Random.float/1` genera un número de punto flotante aleatorio entre 0 y 1 si no se proporciona ningún argumento. Sin embargo, si se proporciona un rango, generará un número de punto flotante aleatorio dentro de ese rango.

Elixir también tiene otras funciones útiles para generar números aleatorios, como `Random.uniform/2`, que genera un número aleatorio dentro de un rango especificado, y `Random.hex/1`, que genera una cadena hexadecimal aleatoria. Estas funciones pueden ser útiles para diferentes situaciones dependiendo de la aplicación.

## Ver también
- [Documentación oficial de Elixir sobre generación de números aleatorios](https://hexdocs.pm/elixir/Random.html)
- [Artículo de blog sobre generación de números aleatorios en Elixir](https://happyteamlabs.com/blog/generating-random-numbers-with-elixir/)