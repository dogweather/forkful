---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La impresión de depuración es una herramienta esencial para los programadores para entender cómo las distintas partes de su código trabajan juntas en tiempo real. Los programadores la usan para detectar y resolver problemas de forma rápida y sistemática.

## Cómo hacerlo:

Aquí hay un ejemplo simple de cómo imprimir el contenido de una variable en tu consola en C++.
```C++
#include <iostream>
 
int main() {
    int numero = 15;
    std::cout << "El número es: " << numero << std::endl;
    return 0;
}
```
En este caso, el contenido se imprimirá en tu consola de la siguiente manera.
```C++
El número es: 15
```

## Profundización:

Historicamente, la impresión de depuración ha sido una práctica común en la programación desde el inicio de los lenguajes de alto nivel, incluyendo la primera versión de C++. Aunque puede parecer primitiva, sigue siendo una práctica común debido a su simplicidad y eficiencia.

Para alternativas a la impresión de depuración, se puede considerar el uso de un depurador de código fuente, el cual permite al programador detener la ejecución del programa en puntos específicos y examinar los valores de las variables y el flujo de control. Sin embargo, los depuradores pueden ser más complicados y pesados para usos rápidos y sencillos.

Siguiendo con detalles más específicos, puedes usar el objeto 'cerr' de la biblioteca iostream para imprimir mensajes de error a la salida estándar de error. 

## Ver también:

- [Tutorial oficial de C++](http://www.cplusplus.com/doc/tutorial/)
- [C++ Reference](http://www.cplusplus.com/reference/)
- [GUÍA RÁPIDA C++](https://developerinsider.co/c-guia-rapida-para-principiantes/)
  
Recuerda siempre que la impresión de depuración es solo una de las muchas herramientas a tu disposición. Utilízala con discreción y siempre busca la manera más eficiente de resolver los problemas de programación.