---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "TypeScript: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Existen varias razones por las cuales uno podría necesitar enviar una solicitud HTTP con autenticación básica utilizando TypeScript. Una de las principales razones es para acceder a una API que requiere autenticación para proteger datos sensibles.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en TypeScript, utilizamos la clase `http` de Node.js y la función `request` para crear la solicitud. Primero, debemos importar estas clases en nuestro archivo.

```TypeScript
import * as http from 'http';
```

Luego, creamos un objeto con las opciones de nuestra solicitud, incluyendo la URL, el método y los encabezados de autenticación.

```TypeScript
const options = {
    hostname: 'www.ejemplo.com',
    path: '/datos',
    method: 'GET',
    auth: 'usuario:contraseña'
};
```

En este ejemplo, estamos utilizando el método GET y agregando nuestras credenciales en la propiedad `auth` en formato de usuario:contraseña. Luego, creamos nuestra solicitud HTTP utilizando la función `request` y pasando nuestras opciones como parámetros.

```TypeScript
const solicitud = http.request(options, respuesta => {
    // Aquí podemos manejar la respuesta de la solicitud
});

solicitud.end(); // Terminamos la solicitud
```

Finalmente, podemos enviar la solicitud utilizando el método `end()` y manejar la respuesta en la función de callback utilizando el objeto `respuesta`.

## Profundizando

Enviar una solicitud HTTP con autenticación básica es una forma común de proteger el acceso a una API. La autenticación básica utiliza un esquema de autenticación simple donde las credenciales se envían en texto plano a través de los encabezados de la solicitud. Sin embargo, debido a que las credenciales no están encriptadas, se considera una forma débil de autenticación y se debe utilizar en combinación con otras medidas de seguridad.

## Ver también

- [Manejo de solicitudes HTTP en TypeScript](https://www.ejemplo.com/manejo-solicitudes-http-typescript/)
- [Documentación de la clase HTTP de Node.js](https://nodejs.org/api/http.html)
- [Tutorial de autenticación básica con Node.js](https://www.ejemplo.com/autenticacion-basica-nodejs/)