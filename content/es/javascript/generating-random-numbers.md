---
title:                "Javascript: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

#¿Por qué generar números aleatorios?

Generar números aleatorios es útil en muchos casos en la programación de Javascript. Puede utilizarse para crear contraseñas seguras, seleccionar elementos aleatorios de una lista, simular juegos de azar, entre otros usos. Además, agregar un elemento de aleatoriedad a un programa puede hacerlo más interesante y emocionante.

## Cómo hacerlo

Para generar números aleatorios en Javascript, podemos utilizar la función ```Math.random ()```, que devuelve un número aleatorio entre 0 (inclusive) y 1 (exclusivo). Luego, podemos combinar esto con otros métodos para obtener un número con un rango específico o con un cierto número de decimales.

A continuación, se muestra un ejemplo de cómo generar un número entero aleatorio entre 1 y 10:

```Javascript
let numero = Math.floor(Math.random() * 10) + 1;
console.log(numero);
// Output: un número entre 1 y 10 (puede variar)
```

Para obtener un número aleatorio con un cierto número de decimales, podemos utilizar el método ```toFixed ()```, que devuelve un string con los decimales especificados. Por ejemplo:

```Javascript
let numero = Math.random().toFixed(2);
console.log(numero);
// Output: un número con dos decimales (puede variar)
```

## Profundizando

Aunque la función ```Math.random ()``` es sencilla y efectiva, no es verdaderamente aleatoria ya que utiliza un algoritmo para generar los números. Además, los números generados pueden repetirse y no son realmente aleatorios en un sentido matemático. Si se necesita una aleatoriedad más estricta, se pueden utilizar librerías externas como "Seedrandom" o "Random.js".

También es importante tener en cuenta que el uso de números aleatorios en programas de Javascript puede tener implicaciones de seguridad si se utilizan para tomar decisiones críticas o para generar valores de autenticación. Es recomendable investigar y comprender cómo se generan los números aleatorios en cada caso y si es necesario utilizar métodos más seguros.

# Ver también

- [Documentación oficial de Math.random ()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Math/random)
- [Seedrandom: biblioteca para generar números aleatorios más seguros](https://github.com/davidbau/seedrandom)
- [Random.js: librería para generar números aleatorios en diferentes formatos](https://chancejs.com/)