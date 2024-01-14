---
title:    "Elixir: Generando números aleatorios"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Por qué generar números aleatorios en Elixir

Si eres un programador de Elixir o estás interesado en aprender sobre este lenguaje de programación funcional, es posible que te hayas preguntado por qué es importante generar números aleatorios en tus aplicaciones. En este post, te explicaré por qué es útil y te mostraré cómo hacerlo en Elixir.

## Cómo generar números aleatorios en Elixir

Generar números aleatorios en Elixir es bastante sencillo gracias a la función `:rand.uniform/1`. Esta función toma un número entero como argumento y genera un número aleatorio entre 0 y el número especificado. Por ejemplo:

```elixir
rand.uniform(10)
```

Este código generará un número aleatorio entre 0 y 10, como 6 o 9. Es importante tener en cuenta que esta función utiliza un generador de números pseudoaleatorios, lo que significa que los números no son verdaderamente aleatorios, sino que siguen un patrón predecible. Sin embargo, para la mayoría de los casos, esto no es un problema y podrás obtener la aleatoriedad suficiente para tus aplicaciones.

## Profundizando en la generación de números aleatorios

Si deseas tener un control más preciso sobre la generación de números aleatorios, Elixir te ofrece la función `:rand.seed/0`, que te permite establecer una semilla para el generador de números aleatorios. Esto es útil si necesitas repetir una secuencia de números aleatorios para pruebas o si deseas garantizar una secuencia específica para la aleatoriedad. Además, Elixir también cuenta con otras funciones para generar números aleatorios en diferentes formatos, como `:rand.float/1` para números decimales aleatorios o `:rand.choice/1` para elegir un elemento aleatorio de una lista.

# Ver también

- [Documentación de Elixir sobre generación de números aleatorios](https://hexdocs.pm/elixir/1.12/Kernel.SpecialForms.html#rand/0)
- [Tutorial de Elixir para principiantes: generación de números aleatorios](https://www.tutorialspoint.com/elixir/elixir_random.htm)
- [Ejemplos de uso de generación de números aleatorios en Elixir](https://www.tysonmaly.com/posts/elixir-random-number-generation-functionality/)