---
title:    "C++: Generación de números aleatorios"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en programación

En la programación, a menudo necesitamos generar números aleatorios para simular situaciones aleatorias o para crear datos de prueba. Los números aleatorios también pueden ser útiles en la criptografía y la seguridad de datos. En esta publicación, exploraremos cómo generar números aleatorios en C++.

## Cómo generar números aleatorios en C++

Para generar números aleatorios en C++, necesitamos incluir la biblioteca estándar `<random>`. Luego, podemos usar la función `rand()` para generar números aleatorios entre 0 y `RAND_MAX`. Sin embargo, esta función no es muy eficiente y no genera números verdaderamente aleatorios. Para una mejor opción, podemos usar la clase `mt19937` de la biblioteca `<random>`. Aquí hay un ejemplo de cómo generar un número entero aleatorio entre 1 y 100:

```C++
#include <random>
#include <iostream>
using namespace std;

int main() {
  random_device rd;
  mt19937 rng(rd());
  uniform_int_distribution<int> uni(1, 100);

  int random_number = uni(rng);
  cout << "El número aleatorio es: " << random_number << endl;

  return 0;
}
```
La salida puede ser algo como: `El número aleatorio es: 56`.

## Profundizando en la generación de números aleatorios

La clase `mt19937` usa un algoritmo llamado Mersenne Twister para generar números aleatorios de alta calidad. Este algoritmo utiliza una semilla para comenzar a generar números y, si la semilla se configura correctamente, los números generados serán verdaderamente aleatorios. Algunos ejemplos de semillas que podemos usar son `std::random_device` y `std::chrono::high_resolution_clock::now().time_since_epoch().count()`. Además, podemos generar diferentes tipos de números aleatorios con diferentes distribuciones, como `uniform_real_distribution` para números decimales, `binomial_distribution` para números binomiales, entre otros.

## Ver también

* [Documentación de la biblioteca \<random\> en C++](http://www.cplusplus.com/reference/random/)
* [Métodos de generación de números aleatorios en C++](https://www.cplusplus.com/articles/EywTURfi/)
* [Introducción a la criptografía y la seguridad de datos](https://desarrolloweb.com/articulos/criptografia-seguridad-datos-introduccion.html)