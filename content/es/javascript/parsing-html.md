---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El análisis sintáctico (parsing) de HTML es el proceso de analizar el contenido de un documento HTML para entender su estructura. Los programadores lo hacen para manipular o extraer información específica del documento HTML.

## Cómo hacerlo:

Aquí te dejo un ejemplo sencillo de cómo podemos hacerlo utilizando Javascript y la API DOMParser.

```Javascript
let parser = new DOMParser();
let documentoHtml = "<html><body><h1>Hola Mundo!</h1></body></html>";
let doc = parser.parseFromString(documentoHtml, "text/html");
console.log(doc.body.textContent); // "Hola Mundo!"
```

En este código, primero creamos un nuevo objeto DOMParser. Después, utilizamos su método `parseFromString()` para convertir una cadena de HTML en un Documento HTML que Javascript puede entender y manipular. Finalmente, extraemos el contenido de texto del cuerpo y lo imprimimos en la consola.

## Inmersión Profunda:

### Contexto Histórico
Historicamente, el análisis sintáctico de HTML era un trabajo arduo y propenso a errores debido a la naturaleza flexible del lenguaje HTML. Pero con la introducción de las APIs de análisis sintáctico en los navegadores modernos, esto se ha convertido en una tarea más manejable.

### Alternativas
Además de DOMParser, también puedes usar la librería JSDOM para hacer análisis sintáctico de HTML. Esta librería es especialmente útil cuando trabajas con Node.js, ya que DOMParser no está disponible en Node.

### Detalles de la implementación
Cuando se parsea un documento HTML, se crea un árbol de nodos (DOM) que representa el contenido del documento. Este árbol puede ser manipulado usando diferentes métodos y propiedades provistas por el API DOM.

## Ver También:

- API DOMParser: https://developer.mozilla.org/es/docs/Web/API/DOMParser
- Librería JSDOM: https://github.com/jsdom/jsdom
- Documentación del API DOM: https://developer.mozilla.org/es/docs/Web/API/Document_Object_Model