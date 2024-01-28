---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:34:00.647934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en programación puede ser crítico para crear simulaciones, pruebas, criptografía y juegos. En Gleam, es una característica que permite a los desarrolladores introducir imprevisibilidad o simular escenarios del mundo real en sus aplicaciones.

## Cómo hacerlo:

Para generar números aleatorios en Gleam, principalmente se usa la biblioteca `gleam_random`. Esta biblioteca proporciona funciones para generar enteros aleatorios, flotantes, y más. Primero, asegúrate de haber añadido `gleam_random` a tu archivo `rebar.config` o `mix.exs` como una dependencia.

Vamos a sumergirnos en algunos ejemplos:

### Generando un Entero Aleatorio

Para producir un entero aleatorio dentro de un rango especificado, puedes usar la función `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Esta función generará un entero aleatorio entre 1 y 10, inclusive.

### Generando un Flotante Aleatorio

Para obtener un flotante aleatorio, usa la función `float`. Esto genera un flotante entre 0.0 y 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Ejemplo de Salida

Ejecutar estas funciones podría producir salidas tales como:

- Para `generate_random_int()`: `5`
- Para `generate_random_float()`: `0.84372`

Recuerda, cada ejecución podría llevar a salidas diferentes debido a la naturaleza de la aleatoriedad.

## Estudio Profundo

El módulo `gleam_random` implementa un generador de números pseudoaleatorios (PRNG), lo que esencialmente significa que los números no son verdaderamente aleatorios pero son difíciles de predecir, emulando la aleatoriedad. Los PRNGs operan empezando con un valor inicial, conocido como la semilla, y aplicando operaciones matemáticas para generar una secuencia de números.

Históricamente, los lenguajes y bibliotecas han implementado varios algoritmos para PRNGs, como el Mersenne Twister o el Generador Lineal Congruencial (LCG). La elección del algoritmo impacta la calidad de la "aleatoriedad", con algunos siendo más adecuados para aplicaciones criptográficas que otros. Aunque la biblioteca estándar de Gleam ofrece conveniencia y facilidad de uso con su módulo `gleam_random`, puede que no siempre sea la mejor elección para casos de uso que requieran aleatoriedad criptográficamente segura. Para propósitos criptográficos, los desarrolladores deberían buscar en bibliotecas diseñadas específicamente para proporcionar generadores de números pseudoaleatorios criptográficamente seguros (CSPRNGs), los cuales están diseñados para resistir ataques que podrían predecir números futuros basados en observar una secuencia de números generados.

En conclusión, mientras que la funcionalidad de generación de números aleatorios de Gleam es robusta para necesidades de programación generales, las aplicaciones con requisitos de seguridad específicos deberían considerar soluciones criptográficas dedicadas para asegurar la integridad y seguridad de su generación de números aleatorios.
