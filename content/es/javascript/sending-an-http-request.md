---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & por qué?

El envío de una solicitud HTTP en Javascript es cuando tu código solicita datos de un servidor. Los programadores lo hacen para obtener o enviar datos de/a ubicaciones web.

## Cómo hacerlo:

Javascript proporciona varias formas de hacer solicitudes HTTP, como `fetch`, `axios` y `XMLHttpRequest`. Aquí te muestro cómo usar `fetch`:

```Javascript
fetch('https://api.miweb.com/datos')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log('¡Error!', error));
```

¡Eso es todo! Ejecutar este código imprimirá los datos recibidos del servidor en tu consola.

## Más a fondo

Historia: La necesidad de enviar solicitudes HTTP ha existido desde que la web empezó a ser interactiva. Inicialmente se usaba XMLHttpRequest, pero con el tiempo se desarrollaron alternativas más modernas y amigables con el desarrollador.

Alternativas: Como mencioné antes, `fetch` es una forma de enviar una solicitud HTTP en Javascript, pero alternativas como `axios` te dan características adicionales, como el soporte de versiones antiguas de navegador, el progreso de la carga y la capacidad para cancelar solicitudes.

Implementación: `fetch` es una Promise, lo que significa que trabaja con operaciones asíncronas. Primero intenta recuperar los datos y luego, si tiene éxito, maneja los datos. Si algo va mal, va al bloque `catch` y maneja el error.

## Ver también

- [Fetch API en MDN](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- [Axios en GitHub](https://github.com/axios/axios)
- [XMLHttpRequest en MDN](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)