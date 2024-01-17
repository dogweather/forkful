---
title:                "Descargar una página web"
html_title:           "Javascript: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# ¿Qué es y por qué hacerlo?

Descargar una página web es obtener el código fuente de una página en la web, incluyendo imágenes, estilo y otros recursos. Los programadores realizan esta tarea para analizar y comprender cómo funciona una página en particular, hacer cambios en ella o utilizar su contenido para sus propios proyectos.

# Cómo hacerlo:

```Javascript
const fetch = require('node-fetch'); // Importar el módulo de Node.js para la descarga de URL

// Definir una función para descargar la página web
async function descargarPagina(url) {
  const respuesta = await fetch(url); // Hacer una petición a la URL
  const html = await respuesta.text(); // Convertir el resultado en texto

  console.log(html); // Mostrar el código fuente de la página en la consola
  
  // Aquí puedes hacer lo que quieras con el código descargado
}
```

# Profundizando más:

- **Contexto histórico:** Descargar páginas web ha sido una práctica común desde los inicios de la web, ya sea para fines de investigación o para el desarrollo de herramientas.
- **Alternativas:** Además de utilizar el módulo de Node.js, también existen otras herramientas y librerías para descargar páginas web, como Puppeteer o la API de descarga de JavaScript.
- **Detalles de implementación:** El código mostrado utiliza la función async/await, una característica agregada en JavaScript para trabajar con promesas de manera más sencilla y intuitiva.

# Ver también:

- [Node.js Fetch](https://nodejs.org/api/fetch.html)
- [Puppeteer](https://pptr.dev/)
- [API de descarga de JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)