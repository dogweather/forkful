---
title:                "Javascript: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración es una herramienta fundamental para los programadores en Javascript. Al incluir mensajes específicos en nuestro código, nos permite entender mejor cómo se está ejecutando nuestro programa y solucionar posibles errores de manera más efectiva.

## Cómo hacerlo

Para imprimir mensajes de depuración en Javascript, utilizaremos el método `console.log()` que nos proporciona el navegador. Este método nos permite imprimir cualquier tipo de dato en la consola, ya sea una cadena de texto, un número o incluso objetos y arreglos.

Veamos un ejemplo:

```Javascript
let num = 5;
console.log("El número es: ", num);
```

En este caso, al ejecutar nuestro código, veremos en la consola el siguiente resultado:

```
El número es: 5
```

También podemos imprimir múltiples variables en un solo mensaje de depuración:

```Javascript
let nombre = "María";
let apellido = "Pérez";
console.log("Mi nombre es ", nombre, " y mi apellido es ",apellido);
```

La consola mostrará:

```
Mi nombre es María y mi apellido es Pérez
```

Además de `console.log()`, también podemos utilizar otros métodos para imprimir mensajes de depuración, como `console.warn()` para mostrar mensajes de advertencia o `console.error()` para mostrar errores.

## Profundizando

Una vez que tengamos una mejor comprensión de cómo imprimir mensajes de depuración, podemos profundizar en cómo utilizarlos de manera más efectiva en nuestro código. Por ejemplo, podemos incluir mensajes de depuración en ciertas partes críticas de nuestro programa para entender mejor dónde se pueden estar produciendo errores.

También es importante recordar eliminar todos los mensajes de depuración antes de lanzar nuestro programa a producción, ya que pueden afectar su rendimiento.

## Ver también

- [Documentación de console en MDN](https://developer.mozilla.org/es/docs/Web/API/console)
- [Cómo depurar código en Javascript](https://www.freecodecamp.org/news/how-to-debug-javascript-like-a-pro/)
- [10 consejos para imprimir mensajes de depuración efectivos](https://www.sitepoint.com/javascript-debugging-tips-tricks/)