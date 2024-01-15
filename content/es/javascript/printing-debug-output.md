---
title:                "Imprimiendo salida de depuración"
html_title:           "Javascript: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir mensajes de depuración en JavaScript?

Antes de sumergirnos en cómo imprimir mensajes de depuración en JavaScript, es importante entender por qué esto es una parte crucial del proceso de desarrollo. Imprimir mensajes de depuración nos permite ver qué está sucediendo en nuestro código en tiempo real y nos ayuda a solucionar problemas de manera eficiente. En lugar de adivinar dónde está el error, podemos imprimir mensajes de texto en diferentes partes de nuestro código y así tener una mejor comprensión de lo que está sucediendo. Esto nos ahorra tiempo y esfuerzo en el proceso de depuración.

## ¿Cómo hacerlo?

Para imprimir mensajes de depuración en JavaScript, podemos usar el método `console.log()`. Este método acepta cualquier tipo de dato como argumento y lo imprime en la consola del navegador. Por ejemplo:

```Javascript
let nombre = "Juan";
console.log("¡Hola " + nombre + "!");
```

La salida en la consola sería: `¡Hola Juan!`, lo que nos permite verificar que la variable `nombre` tenga el valor que esperamos.

También podemos imprimir el valor de una variable dentro de un bucle para ver cómo va cambiando en cada iteración:

```Javascript
for (let i = 0; i <= 10; i++) {
  console.log("El valor de i es: " + i);
}
```

La salida en la consola será:

```
El valor de i es: 0
El valor de i es: 1
El valor de i es: 2
El valor de i es: 3
El valor de i es: 4
El valor de i es: 5
El valor de i es: 6
El valor de i es: 7
El valor de i es: 8
El valor de i es: 9
El valor de i es: 10
```

## Profundizando

Además de `console.log()`, existen otras formas de imprimir mensajes de depuración en JavaScript. Podemos usar `console.warn()` para imprimir mensajes de advertencia, `console.error()` para imprimir mensajes de error, y `console.info()` para imprimir mensajes de información.

También podemos usar `console.table()` para imprimir una tabla con los datos de un objeto o arreglo, y `console.dir()` para imprimir un objeto con sus propiedades y métodos. Estos métodos nos ayudan a visualizar mejor los datos y objetos en lugar de simplemente imprimirlos en forma de cadena de texto.

En general, imprimir mensajes de depuración en JavaScript es una práctica muy útil y necesaria en el desarrollo de aplicaciones. Nos permite entender mejor nuestro código y solucionar problemas rápidamente. Así que ¡no tengas miedo de usar `console.log()` en tu código!

## Ver también

- [Documentación de Console en MDN](https://developer.mozilla.org/es/docs/Web/API/Console) 
- [Debugging JavaScript con Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/javascript)