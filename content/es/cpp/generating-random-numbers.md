---
title:    "C++: Generación de números aleatorios"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

##¿Por qué generar números aleatorios en C++?

Al escribir programas en C++, es posible que en algún momento necesitemos generar números aleatorios. Ya sea para crear juegos, simular situaciones o realizar pruebas, la generación de números aleatorios es una herramienta esencial en la programación. En esta publicación, exploraremos cómo generar números aleatorios en C++ y por qué es importante.

## Cómo hacerlo

La generación de números aleatorios en C++ se basa en una función incorporada llamada `rand()`, que devuelve un número entero aleatorio cada vez que se llama. Sin embargo, para obtener resultados más precisos y controlar el rango de los números generados, podemos utilizar otras funciones y técnicas.

```C++
// Incluir la biblioteca de funciones para generación de números aleatorios
#include <cstdlib> 

// Utilizar la función srand() para sembrar la secuencia de números aleatorios
// Ingrese un número entero como semilla para que los números generados sean diferentes cada vez
srand(1234); 

// Utilizar la función rand() para generar un número aleatorio en un rango específico
int random_num = rand() % 100; // Generará un número aleatorio entre 0 y 99
```

Para obtener más control sobre los números generados, podemos utilizar la biblioteca `random` que ofrece C++, que incluye funciones para generar números aleatorios en diferentes rangos, distribuciones y tipos de datos.

```C++
// Incluir la biblioteca de geración de números aleatorios
#include <random>

// Crear un objeto de tipo random engine
std::default_random_engine generator; 

// Utilizar una distribución normal para generar un número aleatorio tipo double
std::normal_distribution<double> distribution(5.0, 2.0); // Generará un número aleatorio con una media de 5 y una desviación estándar de 2

// Utilizar el objeto del motor para generar el número aleatorio
double random_num = distribution(generator);
```

Con estas funciones y técnicas, podemos generar números aleatorios de manera más precisa y controlada en nuestros programas.

## Profundizando en la generación de números aleatorios

Para aquellos interesados en sumergirse más en el mundo de la generación de números aleatorios en C++, existen muchos conceptos y algoritmos que se pueden explorar. Algunos de ellos incluyen:

- Semillas y su importancia en la generación de números aleatorios
- Distribuciones estadísticas y cómo afectan a los números generados
- Comparación de las diferentes funciones y técnicas para generar números aleatorios en C++

## Ver también

- [Librería <random> en C++ (en inglés)](https://www.cplusplus.com/reference/random/)
- [Tutorial sobre generación de números aleatorios en C++ (en español)](https://programandala.net/es.program.random_in_c.plus.plus.html)
- [Ejemplos de generación de números aleatorios en C++ (en inglés)](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)