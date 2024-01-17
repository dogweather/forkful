---
title:                "Desglosando html"
html_title:           "Javascript: Desglosando html"
simple_title:         "Desglosando html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
El "parsing" es un término técnico que se refiere a la tarea de analizar y procesar datos en un formato específico. En el contexto de la programación de páginas web, el "parsing HTML" se refiere a la tarea de interpretar y manipular el código HTML de una página web. Los programadores realizan esta tarea para poder extraer y utilizar la información contenida en dicha página web.

## Cómo hacerlo:
El parsing HTML se puede hacer utilizando JavaScript, ya que es el lenguaje de programación utilizado para crear interactividad en páginas web. A continuación, se presentan dos ejemplos de cómo parsear HTML en JavaScript:
```Javascript
// Ejemplo 1: Utilizando el método fetch() para obtener los datos HTML de una URL
fetch('https://www.ejemplo.com/pagina')
  .then(response => response.text()) // convierte el contenido a texto
  .then(data => {
    // utilizamos el objeto DOMParser para parsear el HTML y acceder a su contenido
    const parser = new DOMParser();
    const htmlDocument = parser.parseFromString(data, "text/html");
    const titleElement = htmlDocument.querySelector('h1'); // accedemos al primer elemento h1 en la página
    console.log(titleElement.textContent); // imprimimos el contenido del elemento en la consola
  });

// Ejemplo 2: Utilizando la librería Cheerio para parsear el HTML de una página
const cheerio = require('cheerio');
const html = '<ul><li>Elemento 1</li><li>Elemento 2</li></ul>';
const $ = cheerio.load(html); // cargamos el HTML utilizando Cheerio
const listElements = $('li'); // seleccionamos todos los elementos li en la página
listElements.each((index, element) => {
  console.log(element.text); // imprimimos el contenido de cada elemento en la consola
});
```

## Profundizando:
El parsing HTML es una tarea importante en el desarrollo de páginas web, ya que permite a los programadores acceder y manipular el contenido de una página y utilizarlo de diversas maneras. Antes de la creación de herramientas como DOMParser o Cheerio, se utilizaban métodos más rudimentarios para parsear HTML, como el uso de expresiones regulares. Sin embargo, estas técnicas no eran eficientes y podían generar errores. Gracias a las nuevas herramientas, parsear HTML en JavaScript es mucho más sencillo y preciso.

## Ver también:
- [Documentación oficial de DOMParser](https://developer.mozilla.org/es/docs/Web/API/DOMParser)
- [Documentación oficial de Cheerio](https://cheerio.js.org/)