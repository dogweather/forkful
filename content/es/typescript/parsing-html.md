---
title:                "Analizando HTML"
html_title:           "TypeScript: Analizando HTML"
simple_title:         "Analizando HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en crear aplicaciones web interactivas, entonces seguramente te encontrarás en algún momento con la necesidad de analizar y manipular el código HTML de una página web. Algunas posibles razones para hacer esto pueden ser extraer información específica de una página, cambiar el estilo de un elemento o simplemente mejorar la usabilidad de tu aplicación.

## Cómo hacerlo

Para realizar esta tarea en TypeScript, podemos utilizar la combinación de las librerías node-html-parser y axios. Primero, instalamos ambas librerías a través de npm con el comando:

```
npm install --save node-html-parser axios
```

Luego, importamos las librerías en nuestro archivo TypeScript y hacemos una petición HTTP a la página web que queremos analizar:

```TypeScript
import * as parser from 'node-html-parser'; // Importamos la librería de análisis HTML
import axios from 'axios'; // Importamos la librería para hacer peticiones HTTP

axios.get('https://www.ejemplo.com')
    .then((res) => {
        const html = res.data; // Obtenemos el código HTML de la página
    })
    .catch((err) => {
        console.log(err); // Manejo de errores en caso de que la petición falle
    });
```

Una vez que tenemos el código HTML, podemos pasarlo por la función `parser.parse` para obtener un objeto que representa el árbol de elementos de la página:

```TypeScript
const root = parser.parse(html); // Parseamos el HTML y lo guardamos en una variable
```

A partir de este objeto, podemos realizar diferentes operaciones como buscar un elemento específico utilizando su etiqueta, clase o ID, modificar su contenido o incluso añadir nuevos elementos al árbol.

## Profundizando

Si queremos aprender más sobre cómo funciona el proceso de análisis de HTML, podemos echar un vistazo al código fuente de la librería node-html-parser. También podemos investigar sobre diferentes técnicas de scraping, que es precisamente lo que estamos haciendo cuando analizamos y extraemos información de una página web.

## Ver también

- [node-html-parser en GitHub](https://github.com/taoqf/node-html-parser)
- [axios en GitHub](https://github.com/axios/axios)
- [Tutorial de TypeScript en Show.js](https://show.jugernaut.com/typescript-para-principiantes/)