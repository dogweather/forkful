---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:21.794194-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Generar números aleatorios es el acto de crear un número que no se puede predecir razonablemente más allá del azar. Los programadores usamos números aleatorios para todo, desde juegos hasta simulaciones y en la seguridad de nuestros sistemas. 

## Cómo hacerlo:
```gleam
import gleam/erlang
import gleam/int

pub fn main() {
  let seed = erlang.system_time() // Utilizamos el tiempo del sistema como semilla
  let {random_number, _next_seed} = int.rand(seed) // Generamos el número aleatorio
  random_number
}
```
Salida de muestra:
```
235711 // El número generado variará en cada ejecución
```

## Inmersión profunda
Históricamente, generar números aleatorios ha sido un desafío, lo que llevó al desarrollo de algoritmos de números pseudoaleatorios (PRNGs). En Gleam, que corre en la Erlang VM (BEAM), se usa el módulo `rand` de Erlang para generar números aleatorios. Esto tiene muchas aplicaciones prácticas, pero es esencial recordar que los PRNGs no son adecuados para todos los propósitos debido a su naturaleza predecible. Para criptografía, por ejemplo, necesitarías un generador de números aleatorios criptográficamente seguro (CSPRNG).

Alternativas a los números aleatorios incluyen el uso de fuentes de entropía más impredecibles, como la interferencia atmosférica o el ruido térmico, pero eso normalmente está fuera del alcance de la programación a nivel de aplicación.

Detalles de implementación: en Gleam, interactuamos con funcionalidades de Erlang directamente, dado que Gleam se compila en código Erlang. Esto significa que cualquier mejora o cambio en la API de Erlang afectará cómo generamos números aleatorios en Gleam.

## Ver también
- Documentación de Gleam: [https://gleam.run](https://gleam.run)
- Documentación de Erlang's `rand` module: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)