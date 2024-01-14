---
title:    "Gleam: Generación de números aleatorios"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios con Gleam

Muchos desarrolladores utilizan números aleatorios en sus programas para diversas tareas, como generar contraseñas seguras, realizar pruebas de unidad o incluso crear juegos. En esta publicación del blog, aprenderemos cómo generar números aleatorios en Gleam y cómo podemos utilizarlos en nuestros proyectos.

## Cómo generar números aleatorios en Gleam

Para generar números aleatorios en Gleam, utilizaremos la función `random` del módulo `Random`. Esta función toma un parámetro opcional `seed`, que podemos usar para obtener números aleatorios predecibles. Si no proporcionamos un `seed`, la función utiliza un valor predefinido por el sistema.

```
Gleam import Random

seed1 = Random.random(10)
// Esto generará un número aleatorio entre 0 y 10.

seed2 = Random.random()
// Esto generará un número aleatorio utilizando el valor predefinido del sistema.

print(seed1)
// 3

print(seed2)
// 7
```

Podemos utilizar la función `uniform` del módulo `Random` para generar números aleatorios con decimales.

```
Gleam import Random

random_num = Random.uniform(3, 8)
print(random_num)
// 5.2645 
```

## Inmersión profunda en la generación de números aleatorios

Ahora que sabemos cómo generar números aleatorios en Gleam, es importante entender cómo y por qué funciona esto. La función `random` utiliza un algoritmo llamado Generador de Números Pseudoaleatorios (PRNG, por sus siglas en inglés). Este algoritmo se basa en un valor inicial, o `seed`, que se va actualizando con una fórmula matemática cada vez que se llama a la función. Esto garantiza que los números aleatorios generados sean predecibles si se utilizan los mismos valores iniciales (o `seeds`).

Es importante tener en cuenta que, aunque los números generados sean "aleatorios", en realidad son pseudoaleatorios, lo que significa que se generan en base a un algoritmo y no son verdaderamente aleatorios.

## Ver también

- [Documentación del módulo Random en Gleam](https://gleam.run/modules/rand.html)
- [Explicación técnica de los PRNGs](https://computer.howstuffworks.com/question697.htm)
- [Ejemplo de generador de contraseñas aleatorias en Gleam](https://github.com/gleam-lang/example-random-password-generator)