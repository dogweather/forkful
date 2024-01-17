---
title:                "Impresión de salida de depuración"
html_title:           "C++: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué lo hacen los programadores?

Imprimir mensajes de depuración (o debug output) es una técnica utilizada por los programadores para verificar el funcionamiento de su código y encontrar posibles errores. Este tipo de mensajes se escriben en la consola o en un archivo de registro y proporcionan información importante sobre el estado del programa durante su ejecución.

Los programadores utilizan mensajes de depuración para facilitar la tarea de identificar y solucionar problemas en sus programas. Al imprimir mensajes en puntos específicos del código, pueden ver qué variables están almacenando, qué condiciones se están evaluando y dónde puede estar ocurriendo un error. Esto ayuda a ahorrar tiempo y esfuerzo en el proceso de depuración.

## Cómo hacerlo:

```
#include <iostream>

int main() {
    int x = 5;
    std::cout << "El valor de x es: " << x << std::endl; // mensaje de depuración
    return 0;
}

/* Output:
El valor de x es: 5
*/
```

El ejemplo anterior muestra cómo imprimir un mensaje de depuración en C++. Primero se incluye la biblioteca iostream, que proporciona funcionalidades de entrada y salida. Luego, se utiliza la función `cout` para imprimir el mensaje en la consola y se utiliza `endl` para añadir un salto de línea al final del mensaje.

Otra opción común es utilizar la macro `assert` para imprimir mensajes de depuración en caso de que se produzca un error en una determinada sección de código. Esta macro toma como argumentos una expresión lógica y un mensaje de error que se imprimirá si la expresión resulta falsa.

```
#include <cassert>

int main() {
    int x = 5;
    assert(x > 10 && "El valor de x es menor que 10"); // mensaje de depuración
    return 0;
}

/* Output:
Assertion failed: (x > 10 && "El valor de x es menor que 10"), function main, file example.cpp, line 6.
*/
```

## Profundizando:

Históricamente, imprimir mensajes de depuración era una técnica muy utilizada en programación. Sin embargo, con el avance de las herramientas de depuración y los lenguajes de programación más modernos, ha perdido algo de relevancia. Aunque sigue siendo una práctica común y útil, existen otras alternativas, como el uso de depuradores visuales y pruebas unitarias.

La impresión de mensajes de depuración también puede afectar al rendimiento de los programas, especialmente en entornos de producción.  Por esta razón, se recomienda utilizar librerías específicas para depuración o deshabilitar los mensajes de depuración en código compilado en producción.

## Ver también:

- [Mensajes de depuración en C++: cómo hacer que sean más fáciles de leer](https://platzi.com/blog/mensajes-de-depuracion-en-c-como-hacer-que-sean-mas-faciles-de-leer/)
- [Guía para la depuración de código en C++](https://www.drdobbs.com/testing/ultimate-c-debugging-guide-getting-star/225900056)
- [Introducción a la depuración en C++](https://www.forward.com.au/pfod/CPlusPlus/Debug/Introduction.html)