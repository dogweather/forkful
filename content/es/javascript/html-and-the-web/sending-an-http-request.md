---
title:                "Enviando una solicitud http"
date:                  2024-01-20T17:59:56.150030-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Enviar una solicitud HTTP es cómo tu programa habla con el mundo: pide datos a un servidor o les dice qué hacer. Los programadores lo hacen para interactuar con APIs, servicios web y actualizar información en tiempo real.

## Cómo hacerlo:
Para solicitudes HTTP en JavaScript, `fetch` es tu amigo. Puedes obtener datos así:

```Javascript
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

Si necesitas enviar información, usa el método `POST`:

```Javascript
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({key: 'value'}),
})
.then(response => response.json())
.then(data => console.log('Success:', data))
.catch(error => console.error('Error:', error));
```

La respuesta será lo que el servidor te devuelva, típicamente un JSON.

## Profundizando:
Antes de `fetch`, `XMLHttpRequest` era la base para hacer este tipo de tareas. Aunque aún es válido, `fetch` ofrece una manera más moderna y promesas para manejar solicitudes y respuestas asincrónicas.

Existen alternativas como `axios`, una biblioteca que simplifica más las cosas y puede manejar errores de una mejor manera. Sin embargo, `fetch` viene con todos los navegadores modernos, así que no necesitas descargar nada extra.

Detalles de implementación: `fetch` devuelve una Promesa. Es fundamental entender cómo funcionan las Promesas en JavaScript para manejar adecuadamente las respuestas async.

## Ver También:
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- [JavaScript.info - Fetch](https://javascript.info/fetch)
- [Axios on GitHub](https://github.com/axios/axios)
