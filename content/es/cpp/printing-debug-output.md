---
title:    "C++: Imprimiendo salida de depuración"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué usar la impresión de salida de depuración en C++

La impresión de salida de depuración es una técnica muy útil en programación que consiste en imprimir mensajes o valores en pantalla durante la ejecución de un programa. Esto puede ser útil para verificar el flujo del programa, detectar errores o encontrar problemas en el código. En esta publicación, exploraremos cómo utilizar esta técnica en C++ y por qué es importante para cualquier programador.

## Cómo hacerlo

La impresión de salida de depuración en C++ se puede realizar fácilmente utilizando la función `cout` de la librería estándar `iostream`. Esta función permite imprimir en pantalla cualquier tipo de dato, ya sea una variable, una cadena de texto o incluso una expresión matemática. Aquí hay un ejemplo de código que muestra cómo imprimir el valor de una variable `x` en pantalla:

```C++
#include <iostream>
using namespace std;

int main() {

    int x = 5;
    
    // Imprimimos el valor de x en pantalla
    cout << "El valor de x es: " << x << endl;

    return 0;
}
```

El resultado de este código sería: `El valor de x es: 5`. Como se puede ver, la función `cout` nos permite imprimir mensajes y valores en la misma línea, y podemos utilizar el operador de inserción `<<` para separarlos.

## Profundizando en la impresión de salida de depuración

La impresión de salida de depuración no solo es útil para verificar el valor de una variable en un punto específico del programa, también puede ser utilizada para mostrar mensajes de error o para seguir el flujo del programa. Esto puede ser especialmente útil al trabajar en programas más complejos o cuando se encuentran bugs difíciles de resolver.

Una práctica común es utilizar la impresión de salida de depuración dentro de una sentencia `if`, para imprimir un mensaje o valor solo si se cumple una determinada condición. También se puede utilizar dentro de un bucle `for` o `while` para imprimir el valor de una variable en cada iteración y rastrear sus cambios.

Otra técnica útil es utilizar la función `cerr` en lugar de `cout`. La diferencia es que `cerr` imprime en la salida de error estándar, lo que permite distinguir entre mensajes de depuración y mensajes de error en la consola.

## Ver también

- [Depuración de programas en C++](https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_C%2B%2B/Depuraci%C3%B3n_de_programas)
- [Mensajes de depuración con clog](http://www.cplusplus.com/reference/ios/clog/)