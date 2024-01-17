---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Javascript: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Hola a todos los programadores, hoy hablaremos sobre cómo escribir en la "salida de error" en Javascript. Esto se refiere a una forma de imprimir mensajes o información en la consola del navegador de una manera diferenciada de la salida estándar. Los programadores suelen escribir en la salida de error para mostrar mensajes de error o información de depuración que no desea aparecer para el usuario promedio.

## ¿Cómo hacerlo?

Veamos un ejemplo rápido de cómo escribir en la salida de error en Javascript:

```Javascript
console.error("¡Ups! Algo salió mal."); // imprimirá el mensaje de error en la consola del navegador
```

En este ejemplo, estamos utilizando el método `.error()` de la consola para imprimir nuestro mensaje en la salida de error. También puedes utilizar otros métodos como `.warn()` para imprimir advertencias o `.info()` para imprimir información de depuración.

```Javascript
console.warn("¡Cuidado!"); // imprimirá una advertencia en la consola
console.info("La variable i tiene el valor de " + i); // imprimirá información de depuración en la consola
```

Ahora, puedes preguntarte por qué deberías usar la salida de error en lugar de simplemente imprimir en la consola de manera regular. La razón principal es que, en algunos escenarios, es importante diferenciar entre los diferentes tipos de mensajes. Por ejemplo, si estás desarrollando una aplicación para usuarios finales, es posible que no quieras que vean los mensajes de error. Pero si sucede algo inesperado, quieres asegurarte de que los mensajes importantes de error se impriman en la salida de error para que puedas detectar y solucionarlos rápidamente.

## Profundizando

La idea de escribir en la salida de error no es algo nuevo en el mundo de la programación. De hecho, proviene de los sistemas operativos Unix y es una práctica común en varios lenguajes de programación. En lugar de imprimir los mensajes en la consola, en Unix, los mensajes se envían a distintos dispositivos, como la salida estándar o la salida de error.

Además de usar el método `.error()` de la consola, también puedes utilizar el objeto `console` para escribir en la salida de error utilizando `process.stderr` en su lugar. Sin embargo, es importante tener en cuenta que esto puede no funcionar en todos los navegadores.

## Ver también

Si te interesa aprender más sobre la salida de error y cómo escribir en ella en otros lenguajes de programación, puedes consultar estos recursos:

- Documentación oficial sobre la consola en MDN: https://developer.mozilla.org/en-US/docs/Web/API/console
- Artículo sobre la salida de error en Node.js: https://nodejs.org/dist/latest-v10.x/docs/api/console.html#console_console_error_data_args
- Guía de errores básicos en Javascript: https://www.digitalocean.com/community/tutorials/an-introduction-to-error-handling-in-javascript