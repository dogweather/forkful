---
title:                "Descargando una página web"
html_title:           "TypeScript: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es el acto de obtener el código fuente de una página web y guardarlo en un archivo en tu computadora. Los programadores hacen esto para acceder a la información de la página web y ser capaces de analizarla, extraer datos, o automatizar ciertas tareas.

## ¡Cómo hacerlo!

Para descargar una página web en TypeScript, podemos utilizar la biblioteca "node-fetch". Primero, asegúrate de tener Node.js instalado en tu computadora. Luego, en tu terminal, puedes instalar la biblioteca con el siguiente comando:

```TypeScript
npm install node-fetch --save
```

Una vez instalada, podemos usarla en nuestro código para descargar una página web. Por ejemplo, si queremos descargar la página principal de Google, podemos hacer lo siguiente:

```TypeScript
const fetch = require('node-fetch');

// URL de la página a descargar
const url = 'https://www.google.com';

// Utilizamos la función fetch para obtener los datos de la página
fetch(url)
    .then(response => response.text()) // Convertimos los datos a texto
    .then(data => console.log(data)); // Imprimimos los datos en la consola
```

Al ejecutar este código, se imprimirá todo el código fuente de la página de Google en la consola.

## Buceo Profundo

Descargar páginas web ha sido una práctica común en la programación desde los inicios de la internet. Originalmente, los programadores utilizaban el protocolo HTTP para acceder y descargar páginas web, pero con el surgimiento de nuevas tecnologías como JavaScript, se ha permitido la descarga de páginas web de manera más robusta y eficiente.

Existen otras formas de descargar páginas web, como utilizando un navegador automatizado como Selenium o utilizando API's de terceros. Sin embargo, descargar páginas web directamente desde el código sigue siendo una opción muy común debido a su simplicidad y control total sobre los datos descargados.

## Ver También

Si quieres saber más sobre la biblioteca "node-fetch" que utilizamos en nuestro ejemplo, puedes visitar su sitio web oficial: [https://www.npmjs.com/package/node-fetch](https://www.npmjs.com/package/node-fetch)

También puedes aprender más sobre Node.js aquí: [https://nodejs.org/es/](https://nodejs.org/es/)