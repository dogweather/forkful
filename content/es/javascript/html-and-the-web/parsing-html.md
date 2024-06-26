---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:40.575760-07:00
description: "C\xF3mo hacerlo: Vamos a analizar HTML usando la API `DOMParser` en\
  \ JavaScript."
lastmod: '2024-03-13T22:44:59.456668-06:00'
model: gpt-4-0125-preview
summary: Vamos a analizar HTML usando la API `DOMParser` en JavaScript.
title: "An\xE1lisis de HTML"
weight: 43
---

## Cómo hacerlo:
Vamos a analizar HTML usando la API `DOMParser` en JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hola, mundo!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Salida: Hola, mundo!
```

Ahora, vamos a capturar algo más específico, como un elemento con una clase:

```Javascript
const htmlString = `<div><p class="saludo">¡Hola, de nuevo!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const saludo = doc.querySelector('.saludo').textContent;
console.log(saludo); // Salida: ¡Hola, de nuevo!
```

## Estudio Profundo
Analizar HTML es tan antiguo como la web. Inicialmente, era cosa de los navegadores: los navegadores analizaban HTML para mostrar páginas web. Con el tiempo, los programadores querían aprovechar este proceso, lo que llevó a APIs como `DOMParser`.

¿Alternativas? Claro. Tenemos bibliotecas como `jQuery` y herramientas como `BeautifulSoup` para Python. Pero el `DOMParser` nativo de JavaScript es rápido y está integrado, no necesita bibliotecas adicionales.

En términos de implementación, cuando analizas HTML con `DOMParser`, crea un objeto `Document`. Piénsalo como un modelo jerárquico de tu HTML. Una vez que lo tienes, puedes navegar y manipularlo tal como lo harías con el DOM de una página web normal.

Aquí está la cosa: el análisis puede tropezar con HTML mal formado. Los navegadores son tolerantes, pero `DOMParser` podría no serlo. Por lo tanto, para tareas complejas o HTML desordenado, las bibliotecas de terceros podrían hacer un mejor trabajo de limpieza.

## Ver También
- Documentos web de MDN sobre la API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/es/docs/Web/API/DOMParser)
- Capacidades de análisis de jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, una implementación rápida, flexible y ligera del núcleo de jQuery para el servidor: [Cheerio.js](https://cheerio.js.org/)
- Para análisis no-JS: la biblioteca BeautifulSoup de Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
