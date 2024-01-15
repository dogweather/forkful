---
title:                "Enviando una solicitud http"
html_title:           "Javascript: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Si estás desarrollando una aplicación web o un servicio en línea, es posible que en algún momento necesites enviar una solicitud HTTP a otro servidor para obtener datos o realizar una acción específica. Esto es especialmente común en aplicaciones que interactúan con API de terceros. En lugar de tener que lidiar con protocolos de red complicados y conexiones de sockets, Javascript proporciona una manera sencilla de enviar y recibir datos a través de solicitudes HTTP.

## Cómo hacerlo

Para enviar una solicitud HTTP en Javascript, puedes utilizar la función `fetch()` o el objeto `XMLHttpRequest`. Ambos métodos son ampliamente utilizados y tienen sus propias ventajas y desventajas.

### Usando `fetch()`

La función `fetch()` es relativamente nueva y ofrece una sintaxis más simple y moderna para enviar solicitudes HTTP. Aquí hay un ejemplo de cómo enviar una solicitud GET a una API y recibir una respuesta en formato JSON:

```Javascript
fetch('https://jsonplaceholder.typicode.com/users')
  .then(response => response.json())
  .then(data => console.log(data));
```

En este ejemplo, utilizamos `then()` para manejar la respuesta de la solicitud. Primero, convertimos la respuesta a formato JSON y luego imprimimos los datos en la consola. También puedes especificar el tipo de solicitud y agregar parámetros en el objeto `fetch()`.

### Usando `XMLHttpRequest`

El objeto `XMLHttpRequest` ha existido desde los primeros días de Javascript y sigue siendo ampliamente utilizado. Aquí hay un ejemplo de cómo enviar la misma solicitud GET anterior utilizando este objeto:

```Javascript
let request = new XMLHttpRequest();
request.open('GET', 'https://jsonplaceholder.typicode.com/users');
request.responseType = 'json';

request.onload = function() {
  console.log(request.response);
};

request.send();
```

En este ejemplo, establecemos la respuesta en formato JSON y manejamos la respuesta en la función `onload()`.

## Profundizando

Si quieres profundizar más en el tema de enviar solicitudes HTTP en Javascript, es importante que entiendas los distintos tipos de metodologías de solicitud, como GET, POST, PUT y DELETE, y cómo usar parámetros en tus solicitudes. También deberías investigar sobre cómo manejar los errores y las respuestas de error en tus solicitudes.

## Ver También

Si quieres aprender más sobre el uso de HTTP en Javascript, aquí hay algunos enlaces útiles:

- [Documentación oficial de fetch()](https://developer.mozilla.org/es/docs/Web/API/Fetch_API/Utilizando_Fetch)
- [Documentación oficial de XMLHttpRequest](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)
- [Artículo sobre cómo utilizar fetch() y XMLHttpRequest juntos](https://www.javascripttutorial.net/javascript-fetch-api/)
- [Tutorial de Codecademy sobre solicitudes HTTP en Javascript](https://www.codecademy.com/learn/introduction-to-javascript)