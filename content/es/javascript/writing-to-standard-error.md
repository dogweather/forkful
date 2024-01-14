---
title:    "Javascript: Escribiendo en el error estándar"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

##Por qué

Escribir a la salida de error estándar es una práctica útil y fundamental en la programación de JavaScript. Al mostrar información de error o depuración en la salida de error estándar, podemos identificar y resolver problemas en nuestro código de manera más eficiente.

##Cómo hacerlo

Para escribir a la salida de error estándar en JavaScript, podemos usar la función `console.error()` de la consola. Esta función toma como argumento un mensaje de error o depuración y lo muestra en la consola del navegador o del entorno en el que se está ejecutando el código.

```Javascript
console.error("¡Error! Algo ha salido mal.");
```

El resultado de este código sería algo como: 

```console
¡Error! Algo ha salido mal.
```

También podemos utilizar la sintaxis `console.error()` para imprimir objetos y variables en la salida de error estándar. Por ejemplo:

```Javascript
const numero = 10;
console.error("El número es: ", numero);
```

El resultado sería:

```console
El número es: 10
```

##Profundizando

Aunque la función `console.error()` es muy útil para mostrar información de error o depuración, no es la única forma de escribir a la salida de error estándar en JavaScript. Podemos utilizar otros métodos, como `process.stderr.write()` en entornos Node.js o `window.onerror()` en navegadores.

También es importante tener en cuenta que la salida de error estándar es diferente de la salida estándar. Mientras que la salida estándar se utiliza para mostrar información general del programa, la salida de error estándar se usa específicamente para mensajes de error o depuración.

##Ver también

- [Documentación de `console.error()` en MDN](https://developer.mozilla.org/es/docs/Web/API/Console/error)
- [Escribir a la salida de error estándar en Node.js](https://nodejs.org/api/process.html#process_process_stderr)
- [Manejo de errores en JavaScript](https://www.w3schools.com/js/js_error_handling.asp)