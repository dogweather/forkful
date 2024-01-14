---
title:                "C++: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por qué obtener la fecha actual en C++

Obtener la fecha actual es una tarea común en la programación ya que puede ser necesaria para diversas aplicaciones. Por ejemplo, puede ser útil para registrar transacciones, mantener una secuencia de eventos o simplemente para fines de seguimiento del tiempo. En este artículo, aprenderemos cómo obtener la fecha actual en C++ y exploraremos un poco más sobre cómo funciona.

## Cómo hacerlo

En C++, hay varias formas de obtener la fecha actual. Una de las formas más comunes es utilizar la librería estándar "ctime". Esta librería contiene funciones como "time()" y "localtime()" que nos ayudarán a obtener la fecha actual.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtener la fecha y hora actual con time()
    std::time_t now = std::time(0);

    // Convertir a una estructura de tipo "tm"
    std::tm *gmt = std::localtime(&now);

    // Imprimir la fecha actual
    std::cout << "La fecha actual es: " <<
        gmt->tm_mday << "/" << gmt->tm_mon + 1 << "/" << gmt->tm_year + 1900 << std::endl;

    return 0;
}
```

Este código primero utiliza la función "time()" para obtener un objeto "time_t" que representa la fecha y hora actuales. Luego, utilizamos la función "localtime()" para convertir este objeto a una estructura de tipo "tm", que contiene información sobre la fecha y la hora. Finalmente, podemos imprimir los componentes de esta estructura para obtener la fecha actual en el formato deseado.

Otra forma de obtener la fecha actual es utilizando la librería <chrono>, que es parte de la librería de plantillas estándar de C++ (STL). Esta librería proporciona una mejor precisión y es más fácil de usar ya que utiliza objetos como "system_clock" y "time_points" para representar el tiempo. A continuación se muestra un ejemplo que utiliza esta librería para obtener la fecha actual:

```C++
#include <iostream>
#include <chrono>

int main() {
    // Obtener la fecha y hora actual
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

    // Convertir a un objeto "tm" con localtime()
    std::time_t time = std::chrono::system_clock::to_time_t(now);
    std::tm *gmt = std::localtime(&time);

    // Imprimir la fecha actual
    std::cout << "La fecha actual es: " << gmt->tm_mday
         << "/" << gmt->tm_mon + 1 << "/" << gmt->tm_year + 1900 << std::endl;

    return 0;
}
```

Aquí, primero utilizamos la función "now()" para obtener el punto de tiempo actual y luego utilizamos la función "to_time_t()" para convertirlo a un objeto de tipo "time_t". Luego, al igual que en el ejemplo anterior, utilizamos "localtime()" para obtener una estructura de tipo "tm" y finalmente imprimimos la fecha actual.

## Profundizando

Ahora que ya sabemos cómo obtener la fecha actual en C++, es importante entender cómo funciona este proceso. En ambos ejemplos, utilizamos la función "localtime()" para obtener una estructura de tipo "tm", que contiene información sobre la fecha y la hora. Esta estructura se basa en el "epoch time", que es un punto de referencia utilizado para calcular fechas y horas. Este punto de referencia es el 1 de enero de 1970 a las 00:00:00 UTC y se puede encontrar en el objeto "tm_epoch".

Además, en el segundo ejemplo, utilizamos la librería <chrono>, que proporciona una mayor precisión en el cálculo del tiempo, ya que utiliza diferentes "clocks" para medir el tiempo, como por ejemplo "system_clock", "steady_clock" y "high_resolution_clock".

## Vea también

- [Artículo de referencia de C++ sobre la librería <ctime>](https://www.cplusplus.com/reference/ctime/)
- [Artículo sobre la librería <chrono> de la STL](https://www.geeksforgeeks.org/chrono-in-c/)
- [Introducción al "epoch time"](https://en.wikipedia.org/wiki/Epoch_(computing))