---
title:                "PHP: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en la programación

Generar números aleatorios es una técnica comúnmente utilizada en programación para simular situaciones de azar o para crear diversidad en un conjunto de datos. Además, puede ser útil en juegos y en la creación de contraseñas seguras.

## Cómo generar números aleatorios en PHP

En PHP, podemos generar números aleatorios utilizando la función `rand()`, que toma dos parámetros: el número mínimo y máximo que queremos generar. Por ejemplo, si queremos obtener un número aleatorio entre 1 y 10, podemos usar el siguiente código:

```PHP
$num = rand(1, 10);
echo "El número aleatorio es: $num";
```

Esto imprimirá un número aleatorio en cada ejecución. También podemos utilizar esta función para generar una lista de números aleatorios utilizando un bucle `for` o `while`.

Otra opción es utilizar la función `mt_rand()`, que utiliza un algoritmo diferente para generar números aleatorios y puede ser más rápido que la función `rand()` en algunas situaciones.

```PHP
// Generar 5 números aleatorios entre 20 y 50
for ($i = 0; $i < 5; $i++) {
  $num = mt_rand(20, 50);
  echo "Número #$i: $num";
}
```

Además, PHP también cuenta con funciones específicas para generar números aleatorios de punto flotante (`mt_rand()`) y para elegir un elemento aleatorio de un array (`array_rand()`).

## Profundizando en la generación de números aleatorios

La generación de números aleatorios no es realmente aleatoria, ya que se basa en un algoritmo que utiliza un valor inicial (llamado "semilla") para calcular los números. Por esta razón, es importante elegir una buena semilla para obtener resultados más imprevisibles.

También es importante tener en cuenta que los números generados por estas funciones no son realmente aleatorios, sino que siguen un patrón predecible. Por lo tanto, no deben ser utilizados en situaciones críticas donde se necesite verdadera aleatoriedad, como en sistemas de seguridad o de criptografía.

## Ver también

- Ejemplos de código de generación de números aleatorios en PHP: https://www.w3schools.com/php/func_math_rand.asp
- Más información sobre la generación de números aleatorios en PHP: https://www.php.net/manual/es/function.mt-rand.php