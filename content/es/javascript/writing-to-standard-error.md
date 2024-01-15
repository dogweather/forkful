---
title:                "Escribiendo en el error estándar"
html_title:           "Javascript: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##¿Por qué escribir en error estándar?

Escribir en error estándar es una práctica común en el mundo de la programación. Puede ser útil en situaciones donde se necesita registrar errores o mensajes importantes que no deberían ser ignorados. También puede ser utilizado para depurar y monitorear el rendimiento de una aplicación.

## Cómo hacerlo:

Para escribir en error estándar en Javascript, puedes utilizar el objeto `console` y su método `error()`. Por ejemplo:

```Javascript
console.error("¡Error! No se pudo encontrar el archivo solicitado.");
```

El resultado en la consola sería:

```
¡Error! No se pudo encontrar el archivo solicitado.
```

Este método también acepta múltiples argumentos, por lo que se pueden incluir variables u otros mensajes en el mismo comando. Puedes utilizar este método en cualquier parte de tu código donde quieras registrar un mensaje de error.

## Profundizando en el tema:

Escribir en error estándar no solo es útil para registrar errores, sino que también te permite monitorear el rendimiento de tu aplicación. Puedes utilizar herramientas de terceros para interceptar y analizar los mensajes de error, lo que puede ayudar a identificar problemas y mejorar el rendimiento de tu código.

Además, al escribir en error estándar, estás siguiendo buenas prácticas de programación y facilitando el trabajo de otros programadores que puedan necesitar depurar o mantener tu código en el futuro.

## Ver También:

- [Documentación de console.error en MDN](https://developer.mozilla.org/es/docs/Web/API/Console/error)
- [Introducción a la escritura en error estándar](https://www.toptal.com/c/standard-error-piping-and-logging-in-node-js)
- [Herramientas para analizar y monitorizar errores en Javascript](https://blog.logrocket.com/javascript-debugging-tricks-tools/)