---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Descargando una página web con JavaScript

## ¿Qué y por qué?

Descargar una página web significa recuperar los datos de esa página desde un servidor y guardarlos localmente. Los programadores a menudo necesitan descargar páginas web para extraer, analizar y utilizar la información en ellas.

## Cómo hacerlo:

Podemos usar el módulo `axios` para descargar una página web en JavaScript. Primero, debes instalarlo. Abre la línea de comando de tu PC y escribe:

```Javascript
npm install axios
```
Ahora puedes descargar y registrar una página web en la consola con este código simple:

```Javascript
const axios = require('axios');

axios.get('https://example.com')
  .then(res => {
    console.log(res.data);
  })
  .catch(err=>{
      console.log(err);
  });
```
Este código descargará el cuerpo HTML de `https://example.com` y lo registrará en la consola.

## Profundizando

La descarga de páginas web, también conocida como web scraping, tiene una larga historia. Antes de los módulos actuales como `axios`, `got`, etc., los programadores solían hacerlo manualmente con lenguajes de bajo nivel.

Existen muchas alternativas para descargar una página web en JavaScript aparte de `axios`. `node-fetch` y `got` son dos de las más populares. Si deseas trabajar con un enfoque más bajo nivel, el módulo `http` de Node.js también puede ser una opción.

Al realizar web scraping, recuerda respetar las directivas `robots.txt` del sitio web y siempre pedir permiso si es posible.

## Ver también

- Documentación oficial de Axios (https://axios-http.com/docs/intro)
- Documentación oficial de Node-fetch (https://www.npmjs.com/package/node-fetch)
- Para profundizar en web scraping: (https://realpython.com/tutorials/web-scraping/)