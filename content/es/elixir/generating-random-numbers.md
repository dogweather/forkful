---
title:                "Elixir: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Elixir es un lenguaje de programación funcional que ofrece una amplia variedad de herramientas y funciones para crear aplicaciones potentes y escalables. Una de estas herramientas es la capacidad de generar números aleatorios, lo que puede ser útil en una variedad de escenarios, desde juegos hasta pruebas de rendimiento.

## Cómo hacerlo
Generar números aleatorios en Elixir es muy sencillo gracias a la función `rand`. Esta función toma dos argumentos: un límite inferior y uno superior, y devuelve un número aleatorio dentro de ese rango. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podríamos hacerlo de la siguiente manera:

```Elixir
rand(1, 10)
```

Si queremos generar un número entero aleatorio, podemos usar `round` para redondear el resultado:

```Elixir
round(rand(50, 100))
```

Y si queremos generar un número de punto flotante aleatorio, podemos usar `Float.round` para redondear el resultado:

```Elixir
Float.round(rand(1.0, 5.0))
```

¡Pero eso no es todo! También podemos generar listas aleatorias utilizando `Enum.shuffle`:

```Elixir
Enum.shuffle([1, 2, 3, 4, 5])
```

También podemos usar `:random.uniform` si queremos generar un número aleatorio sin especificar un rango:

```Elixir
:random.uniform()
```

## Profundizando
La función `rand` en realidad utiliza un generador de números aleatorios subyacente llamado `:random`. Este generador se basa en un algoritmo llamado Congruencia Lineal, que produce una secuencia de números pseudoaleatorios a partir de una semilla.

También podemos especificar una semilla para el generador de números aleatorios utilizando la opción `:seed` en la función `rand` o en el módulo `:random`. Esto puede ser útil si queremos obtener resultados reproducibles o si queremos generar secuencias de números pseudoaleatorios basadas en una semilla específica.

Además, Elixir también ofrece otros módulos para generar números aleatorios, como `:rand`, que ofrece más opciones de configuración para el generador de números aleatorios. Puedes explorar más sobre estos módulos en la documentación oficial de Elixir.

## Ver también
- [Documentación oficial de Elixir sobre números aleatorios](https://hexdocs.pm/elixir/Random.html)
- [Blog post sobre generación de números aleatorios en Elixir](https://blog.lelonek.me/randomness-in-elixir-f8c0dd026883)
- [Ejemplos de uso de números aleatorios en Elixir](https://www.learnelixir.com/blog/random-generation-in-elixir/)