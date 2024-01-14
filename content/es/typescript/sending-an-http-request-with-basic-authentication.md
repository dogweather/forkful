---
title:                "TypeScript: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP con autenticación básica es una forma común y segura de comunicarse entre una aplicación y un servidor. Al incluir credenciales de autenticación en la solicitud, se puede garantizar que solo usuarios autorizados tengan acceso a la información protegida.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en TypeScript, podemos utilizar el paquete `node-fetch` y la función `fetch` incorporada. Primero, importemos el paquete y definamos nuestra URL y credenciales de autenticación:

```TypeScript
import fetch from 'node-fetch';

const url = 'https://example.com/api';
const username = 'username';
const password = 'password';
```

Luego, creamos nuestra solicitud HTTP con las opciones necesarias para la autenticación básica:

```TypeScript
const options = {
  headers: {
    Authorization: `Basic ${Buffer.from(`${username}:${password}`, 'utf8').toString('base64')}` 
  }
};
```

`Buffer.from` se utiliza para crear una cadena codificada en base64 con nuestras credenciales de autenticación. Ahora, podemos enviar la solicitud utilizando la función `fetch` y obtener los datos JSON correspondientes:

```TypeScript
fetch(url, options)
  .then(res => res.json())
  .then(data => {
    console.log(data);
  });
```

La respuesta de esta solicitud será un objeto JSON con la información de la API solicitada.

## Profundizando

Al enviar una solicitud HTTP con autenticación básica, es importante tener en cuenta que las credenciales de autenticación se envían en texto plano, lo que puede ser un riesgo de seguridad. Por lo tanto, se recomienda utilizar HTTPS para cifrar la comunicación entre la aplicación y el servidor.

 Además, también se pueden utilizar otras opciones de autenticación, como OAuth, para aumentar la seguridad de la comunicación entre aplicaciones.

## Ver también

Si estás buscando más información sobre cómo enviar y recibir solicitudes HTTP en TypeScript, aquí tienes algunos recursos útiles:

- [Usando fetch en TypeScript](https://dev.to/harveyio/usando-fetch-en-typescript-1h2c)
- [Node-fetch en GitHub](https://github.com/node-fetch/node-fetch)
- [Documentación de TypeScript sobre tipos de datos](https://www.typescriptlang.org/docs/handbook/basic-types.html)