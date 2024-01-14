---
title:                "Javascript: Envío de una solicitud http"
simple_title:         "Envío de una solicitud http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué enviar una petición HTTP

Enviar una petición HTTP es una parte fundamental del desarrollo web. Nos permite comunicarnos con servidores externos y obtener información que puede ser usada para mostrar contenido dinámico en nuestras aplicaciones. También es esencial para la creación de aplicaciones web que interactúan con bases de datos o servicios externos. Si quieres llevar tus habilidades de programación al siguiente nivel, aprender a enviar una petición HTTP es clave.

## Cómo hacerlo

En Javascript, podemos enviar una petición HTTP utilizando la función `fetch`. Esta función toma como argumento la URL del recurso al que queremos acceder y devuelve una promesa con la respuesta del servidor. Veamos un ejemplo:

```Javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(data => console.log(data));
```

En este código, estamos enviando una petición a la API de JSON placeholder para obtener información sobre el post con el ID 1. Luego, utilizamos el método `json()` para convertir la respuesta en un objeto JSON legible. Finalmente, imprimimos el resultado en la consola.

La salida de este código sería:

```Javascript
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit " +
          "molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
```

Este es solo un ejemplo sencillo de cómo enviar una petición HTTP en Javascript, pero hay muchas formas de hacerlo y diferentes opciones de configuración que pueden ser utilizadas. Asegúrate de investigar y probar diferentes métodos para encontrar el que mejor se adapte a tus necesidades.

## Profundizando

Para enviar una petición HTTP, debemos tener en cuenta diferentes aspectos de la misma, como el método que estamos utilizando (GET, POST, PUT, DELETE), los encabezados o headers que enviamos y recibimos, y el tipo de datos que queremos enviar y recibir. Además, también es importante manejar los errores y posibles respuestas no exitosas del servidor.

Una herramienta muy útil para probar y entender mejor las peticiones HTTP es el uso de una plataforma como Postman. Con esta herramienta, podemos enviar diferentes tipos de peticiones y experimentar con distintas configuraciones para entender mejor cómo funciona el proceso de envío y recepción de datos entre nuestro código y el servidor.

## Ver también

- [Guía de introducción a fetch API](https://developer.mozilla.org/es/docs/Web/API/Fetch_API/Utilizando_Fetch)
- [Documentación de Postman](https://learning.postman.com/docs/getting-started/introduction/)
- [Libro "Eloquent JavaScript" (capítulo sobre peticiones HTTP)](https://eloquentjavascript.net/18_http.html)