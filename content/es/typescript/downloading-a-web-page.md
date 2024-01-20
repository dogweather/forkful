---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Descargar una página web significa copiar los datos de la página al almacenamiento local de tu ordenador. Los programadores lo hacen para analizar el contenido de la página, realizar pruebas de seguridad o para almacenar su contenido y verlo sin conexión.

## Cómo hacerlo:

A continuación, encontrarás un ejemplo simple sobre cómo descargar una página web usando el módulo de `http` de Node.js y TypeScript. 

```TypeScript
import http from 'http';

http.get('http://ejemplo.com', respuesta => {
  let datos = '';

  respuesta.on('data', bloque => {
    datos += bloque;
  });

  respuesta.on('end', () => {
    console.log(datos);
  });
}).on('error', err => {
  console.log(`Error: ${err.message}`);
});
```
Este script solicita la página de "ejemplo.com" y cada bloque de datos que recibe se añade a la variable `datos`. Una vez que todos los datos han llegado, los imprimirá en la terminal. 

## Buceo profundo

Históricamente, descargar páginas web era una tarea relativamente fácil porque las páginas se basaban principalmente en HTML estático. Sin embargo, el desarrollo de JavaScript y la creación de sitios web más dinámicos y ricos en características ha hecho que esta tarea sea más compleja.

Hay varias alternativas a la descarga de páginas web con Node.js y TypeScript. Puedes usar `axios` o `request-promise` para tareas más complejas o incluso módulos como `puppeteer` para interactuar con páginas web JavaScript-heavy en lugar de simplemente descargar su contenido.

Los detalles de implementación pueden variar dependiendo del módulo que uses y de cómo esté configurada la página web. Por ejemplo, si la página emplea técnicas para evitar el scraping, tal vez tengas que simular un navegador legítimo o incluso ejecutar JavaScript en esa página para obtener el contenido que necesitas.

## Ver también

Descargar una página web es solo el primer paso para el scraping de sitios web. Aquí tienes algunos enlaces para profundizar tu conocimiento:

- [MDN Web Docs: HTTP](https://developer.mozilla.org/es/docs/Web/HTTP)
- [Documentación de Node.js: http](https://nodejs.org/api/http.html)
- [Recursos sobre web scraping con Node.js y TypeScript](https://hackernoon.com/web-scraping-tutorial-with-javascript-159)
- [axios, una alternativa al módulo http](https://github.com/axios/axios)
- [puppeteer, un módulo para controlar navegadores](https://pptr.dev/)