---
title:                "Enviando una solicitud http"
aliases:
- /es/typescript/sending-an-http-request/
date:                  2024-01-20T18:00:33.811078-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Mandar una solicitud HTTP es el proceso de pedir o enviar datos a un servidor web. Lo hacemos para interactuar con APIs, obtener recursos, enviar formularios y más - es la base de la comunicación en la red.

## How to:
Para mandar una solicitud HTTP, vamos a usar `fetch`, que está ya incluido en JavaScript y es accesible en TypeScript.

```TypeScript
// GET Request
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(data => console.log(data));

// POST Request
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json));

```
Esto imprimiría en consola la respuesta del servidor en formato JSON.

## Deep Dive
La función `fetch` se introdujo en la web para reemplazar `XMLHttpRequest`, que era más compleja y menos intuitiva. Si necesitas compatibilidad con navegadores antiguos, podrías considerar herramientas como `axios` o `jQuery.ajax`. Además, con TypeScript, es buena idea definir interfaces para las respuestas que esperas, lo que te ayuda a manejar los datos de forma predecible y segura.

## See Also
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- [TypeScript Handbook - Utility Types](https://www.typescriptlang.org/docs/handbook/utility-types.html)
- [JSONPlaceholder - Fake Online REST API](https://jsonplaceholder.typicode.com/)
