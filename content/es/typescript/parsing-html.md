---
title:                "TypeScript: Analizando html"
simple_title:         "Analizando html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Al crear una aplicación web, a menudo se necesita acceder a información específica de una página web. Para lograr esto, se puede utilizar un proceso llamado "parsing" o análisis. En términos sencillos, esto significa extraer datos de una página HTML para su uso en una aplicación.

## Cómo

Para realizar el parsing de HTML en TypeScript, necesitamos utilizar la librería "cheerio". Esta biblioteca nos permite seleccionar elementos en una página web utilizando selectores similares a CSS. Aquí hay un ejemplo básico de cómo parsear una página web y obtener el título:

```TypeScript
import * as cheerio from 'cheerio';
import * as request from 'request';

request('https://www.mipagina.com', (error, response, body) => {
  if (!error && response.statusCode == 200) {
    const $ = cheerio.load(body);
    const title = $('h1').text();
    console.log(title);
  }
});
```

En este ejemplo, utilizamos el módulo "request" para obtener el HTML de la página deseada y luego lo pasamos a Cheerio para que pueda analizarlo. Luego, utilizamos un selector para elegir el elemento "h1" y extraer su texto.

## Deep Dive

Además de obtener datos de una página web, el parsing de HTML también tiene otros usos útiles en aplicaciones web. Al utilizar el análisis de HTML, se pueden realizar tareas como el web scraping, que consiste en extraer datos de múltiples páginas web para su uso en análisis de datos o estudios de mercado.

Hay otros módulos útiles que se pueden utilizar junto con Cheerio para mejorar la calidad de los datos obtenidos, como "request-promise" para obtener un HTML más limpio y "iconv-lite" para manejar diferentes codificaciones de caracteres en la página.

En general, el parsing de HTML en TypeScript es una herramienta poderosa para acceder a datos de una página web y utilizarlos en aplicaciones web y análisis de datos.

## Ver también

- [Cheerio documentation](https://www.npmjs.com/package/cheerio)
- [Request module documentation](https://github.com/request/request)
- [Request-promise module documentation](https://github.com/request/request-promise)
- [Iconv-lite documentation](https://www.npmjs.com/package/iconv-lite)

¡Gracias por leer nuestro artículo sobre el parsing de HTML en TypeScript! Esperamos que haya sido útil para su próximo proyecto de desarrollo web. ¡Feliz coding!