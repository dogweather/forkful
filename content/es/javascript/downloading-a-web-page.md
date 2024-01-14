---
title:                "Javascript: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar un página web es una tarea muy útil si deseas guardar una copia de un sitio en particular para verlo sin conexión o para realizar pruebas en tu propio equipo. También puede ser una forma de aprender cómo están construidos otros sitios web y mejorar tus habilidades de programación.

## Cómo hacerlo

Existen varias formas de descargar una página web utilizando JavaScript. Una de las formas más simples es utilizando la Biblioteca Fetch API que nos permite realizar solicitudes HTTP y descargar contenido de una página web. Veamos un ejemplo:

```Javascript
fetch('https://www.example.com/')
  .then(response => response.text())
  .then(data => console.log(data))
```

En este ejemplo, utilizamos la función `fetch()` para hacer una solicitud GET a la página web que queremos descargar. Luego utilizamos la función `then()` para recibir la respuesta y convertirla en texto utilizando el método `text()`. Finalmente, imprimimos el contenido de la página web en la consola utilizando `console.log()`.

También puedes utilizar la Biblioteca Axios para descargar una página web. Esta biblioteca es similar a Fetch API, pero ofrece más funciones y mejor compatibilidad con navegadores antiguos. Aquí tienes un ejemplo de cómo puedes utilizar Axios:

```Javascript
const axios = require('axios');

axios.get('https://www.example.com/')
  .then(response => console.log(response.data))
  .catch(error => console.log(error));
```

En este ejemplo, primero importamos la biblioteca Axios y luego utilizamos la función `get()` para hacer una solicitud GET a la página web. Luego manejamos la respuesta utilizando la función `then()` y finalmente imprimimos el contenido de la página en la consola.

## Profundizando

Al descargar una página web utilizando JavaScript, es importante tener en cuenta que la respuesta devuelta será el contenido HTML de la página sin ningún tipo de estilo o elementos adicionales. Si deseas descargar todo el contenido de una página web, incluyendo estilos, imágenes y JavaScript, puedes utilizar una biblioteca como Puppeteer.

Puppeteer es una herramienta que te permite controlar y automatizar un navegador web utilizando JavaScript. Con esta herramienta, puedes navegar a una página web específica, interactuar con los elementos de la página y luego guardar el contenido completo de la página en tu equipo. Aquí tienes un ejemplo de cómo puedes hacerlo:

```Javascript
const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto('https://www.example.com/');
  await page.screenshot({ path: 'example.png' });
  await browser.close();
})();
```

En este ejemplo, utilizamos Puppeteer para abrir un navegador y navegar a la página web que queremos descargar. Luego tomamos una captura de pantalla de la página y la guardamos como una imagen en nuestro equipo. Al final, cerramos el navegador.

## Ver también

- [Fetch API](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- [Axios](https://axios-http.com/)
- [Puppeteer](https://pptr.dev/)