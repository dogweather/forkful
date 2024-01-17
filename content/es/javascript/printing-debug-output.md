---
title:                "Imprimiendo resultados de depuración"
html_title:           "Javascript: Imprimiendo resultados de depuración"
simple_title:         "Imprimiendo resultados de depuración"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Imprimir salida de depuración es una técnica utilizada por programadores para ayudar en el proceso de depuración del código. Consiste en mostrar mensajes o valores específicos en la consola del navegador o en un archivo de registro, para verificar el funcionamiento del programa y detectar posibles errores.

## Cómo:
```Javascript
console.log("Mensaje de depuración"); // imprime un mensaje en la consola
console.log(variable); // imprime el valor de una variable
console.log("El resultado es: " + resultado); // imprime un mensaje seguido de un valor
```

**Salida de ejemplo:**
```
Mensaje de depuración
5
El resultado es: 42
```

## Inmersión profunda:
La impresión de salida de depuración tiene sus raíces en los primeros días de la programación, cuando no había herramientas sofisticadas para depurar código. Los programadores tenían que confiar en mensajes en la consola para encontrar y solucionar errores. Hoy en día, existen herramientas más avanzadas como depuradores en vivo que permiten a los programadores detener la ejecución del código y analizarlo en tiempo real.

Otra alternativa a la impresión de salida de depuración es el uso de pruebas unitarias o la depuración interactiva. Estas técnicas pueden ser más efectivas para encontrar errores en casos complejos.

En términos de implementación, la impresión de salida de depuración se logra utilizando el método `console.log()` en Javascript. También hay otros métodos disponibles en la consola, como `console.error()` y `console.warn()`, que imprimen mensajes de error y advertencia, respectivamente.

## Ver también:
- [Artículo de MDN sobre el método console.log()](https://developer.mozilla.org/es/docs/Web/API/Console/log)
- [Uso de pruebas unitarias en la depuración de código](https://www.freecodecamp.org/news/javascript-debugging-tips-for-beginners/)
- [Ejemplo de depuración interactiva en Javascript](https://www.toptal.com/javascript/javascript-debugging-tips-and-tricks)