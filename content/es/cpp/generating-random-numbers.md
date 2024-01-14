---
title:                "C++: Generando números aleatorios"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

El uso de números aleatorios en programación puede ser útil en una variedad de situaciones, como en juegos, sorteos, generación de contraseñas y pruebas de rendimiento. Los números aleatorios también pueden agregar un elemento de sorpresa y diversión a una aplicación.

## Cómo hacerlo

La generación de números aleatorios en C++ es bastante sencilla con la ayuda de la biblioteca estándar <string> y la función rand (). Primero, asegúrese de incluir la biblioteca:

```C++
#include <cstdlib>
```

Luego, para generar un número aleatorio, simplemente llame a la función rand () y asigne el resultado a una variable:

```C++
int randomNumber = std::rand();
```

Para limitar el rango de los números generados, puede utilizar el operador de módulo con el número máximo deseado más 1:

```C++
int randomNumber = std::rand() % 20; // generará un número entre 0 y 19
```

Para obtener una secuencia de números diferentes cada vez que se ejecute el programa, puede utilizar la función srand () para establecer una semilla diferente. Una opción es utilizar el tiempo actual como semilla:

```C++
std::srand(time(0)); // establece la semilla como el tiempo actual
```

Este es un ejemplo completo de un programa que genera 10 números aleatorios entre 1 y 100 y los imprime en la pantalla:

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main() {
    std::srand(time(0));

    for (int i = 0; i < 10; i++) {
        int randomNumber = std::rand() % 100 + 1;
        std::cout << randomNumber << " ";
    }

    return 0;
}
```

El resultado podría ser algo como:

```bash
57 4 76 29 96 12 88 71 62 99
```

## Profundizando

La función rand () en sí no es realmente aleatoria, sino que utiliza un algoritmo para generar una secuencia pseudorandom. Esto significa que, aunque los números parecen aleatorios, realmente son deterministas y se pueden reproducir utilizando la misma semilla.

Para obtener números verdaderamente aleatorios, se puede utilizar una fuente externa, como un dispositivo de hardware especializado o la entrada del usuario. Sin embargo, esto puede disminuir la velocidad de su programa.

Además, en lugar de usar la biblioteca estándar de C++, se pueden explorar otras bibliotecas de generación de números aleatorios más avanzadas, como Boost.Random y C++11 `<random>`. Estas bibliotecas ofrecen una mayor flexibilidad y control sobre la generación de números aleatorios.

## Ver también

- [Documentación de C++ sobre rand ()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Tutorial de random en C++](https://www.learncpp.com/cpp-tutorial/random-number-generation/)
- [Boost.Random](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_random.html)
- [Biblioteca `<random>` en C++11](https://en.cppreference.com/w/cpp/header/random)