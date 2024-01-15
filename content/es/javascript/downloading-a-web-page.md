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

## Por qué

Descargar una página web es una tarea común en el desarrollo web. Ya sea para realizar pruebas, extraer datos o simplemente por curiosidad, tener la habilidad de descargar una página web es una habilidad importante para cualquier programador.

## Cómo hacerlo

Para descargar una página web en Javascript, podemos utilizar la API Fetch. Esta API permite realizar solicitudes HTTP y recibir la respuesta en formato de objeto de respuesta, que incluye tanto el cuerpo de la respuesta como metadatos como el estado y las cabeceras.

```Javascript
fetch("https://www.ejemplo.com")
  .then(response => response.text())
  .then(data => {
    console.log(data);
  })
  .catch(error => {
    console.log(error);
  });
```

En este ejemplo, utilizamos la función `fetch` para realizar una solicitud GET a la dirección proporcionada, y luego usamos el método `then` para obtener el cuerpo de la respuesta en formato de texto. Finalmente, utilizamos `console.log` para imprimir dicho texto en la consola.

Pero, ¿qué pasa si queremos descargar una página web específica de manera recursiva? Por ejemplo, si queremos descargar todas las páginas de un sitio web para realizar un análisis. En ese caso, podemos utilizar una biblioteca externa como Axios para realizar solicitudes HTTP y un módulo integrado en Node.js llamado FS para guardar los archivos descargados en nuestro sistema de archivos.

```Javascript
const axios = require("axios");
const fs = require("fs");

// Función recursiva para descargar una página y todas sus páginas enlazadas
function descargarPaginas(url, ruta) {
  axios.get(url)
    .then(response => {
      fs.writeFileSync(`${ruta}/${url.substring(url.lastIndexOf("/") + 1)}`, response.data);
      // Encontrar todos los enlaces en la página actual
      const enlaces = response.data.match(/href="(.*?)"/g);
      // Descargar cada enlace encontrado
      enlaces.forEach(enlace => {
        descargarPaginas(enlace.substring(6, enlace.length - 1), ruta);
      });
    })
    .catch(error => {
      console.log(error); 
    });
}

// Llamar a la función y especificar la URL y la ruta de descarga deseada
descargarPaginas("https://www.ejemplo.com", "descargas");
```

En este ejemplo, utilizamos la función `escribirFileSync` de FS para guardar el contenido de cada página en un archivo dentro de una carpeta llamada "descargas". Luego, buscamos todos los enlaces en la página actual y llamamos a la función `descargarPaginas` de manera recursiva para descargar cada página enlazada.

## Deep Dive

Además de la API Fetch, existen otras formas de descargar páginas web en Javascript. Por ejemplo, podemos utilizar el módulo Request de Node.js para realizar solicitudes HTTP o, si estamos trabajando en un proyecto de Node.js, podemos utilizar la biblioteca Cheerio para analizar y extraer datos del HTML descargado.

También es importante tener en cuenta que, en algunos casos, puede ser necesario especificar headers o utilizar autenticación para descargar una página web correctamente. En esos casos, debemos leer la documentación o buscar ejemplos específicos en línea para poder tener éxito en nuestra tarea.

## Ver también

- [Fetch API](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- [Axios](https://github.com/axios/axios)
- [FS - Node.js](https://nodejs.org/api/fs.html)
- [Cheerio](https://cheerio.js.org/)