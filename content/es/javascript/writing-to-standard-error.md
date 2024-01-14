---
title:    "Javascript: Escribiendo a Error Estándar"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de error estándar es importante en Javascript?

Escribir a la salida de error estándar es una práctica crucial en Javascript ya que nos permite mostrar información importante sobre los errores o fallos en nuestro código. A través de la salida de error estándar, podemos identificar y corregir rápidamente los errores en nuestro código, lo que nos ayuda a mejorar nuestra capacidad de programación y a crear aplicaciones más robustas y funcionales.

## ¿Cómo escribir a la salida de error estándar en Javascript?

La forma más común de escribir a la salida de error estándar en Javascript es utilizando el método `console.error()`. Este método acepta uno o varios argumentos y los imprime en la salida de error estándar. Veamos un ejemplo:

```Javascript
let num = 5;

if(num > 10) {
  console.log("El número es mayor que 10");
} else {
  console.error("Error: El número es menor que 10");
}
```

En este ejemplo, si el número es menor que 10, se imprimirá el mensaje de error en la consola. También es posible usar plantillas de cadena para imprimir información más detallada:

```Javascript
let nombre = "Juan";
let edad = 25;

console.error(`Error: ${nombre} tiene ${edad} años y no cumple con la edad mínima requerida.`);
```

En este caso, la salida de error sería "Error: Juan tiene 25 años y no cumple con la edad mínima requerida."

## Profundizando en la escritura a la salida de error estándar

Además de mostrar mensajes de error, también podemos utilizar la salida de error estándar para imprimir otra información útil durante el proceso de depuración de nuestro código. Por ejemplo, podemos imprimir el contenido de variables o el resultado de una operación para asegurarnos de que nuestro código se está ejecutando correctamente en cada paso. 

También podemos utilizar el método `console.trace()` para imprimir una pila de llamadas, lo que nos permite rastrear el flujo de nuestro código y identificar dónde se originó un determinado error. 

## Ver también
- [Documentación de console.error() en MDN (en inglés)](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Guía de depuración en Javascript (en español)](https://carlosazaustre.es/manos-a-la-consola-guia-de-depuracion/)
- [Tutorial de Javascript para principiantes (en español)](https://codeburst.io/javascript-tutorial-espa%C3%B1ol-2f3f031ff01c)