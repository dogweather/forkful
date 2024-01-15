---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Javascript: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma segura de proteger la información que se envía entre el servidor y el cliente. Al utilizar este método de autenticación, se requiere un nombre de usuario y una contraseña para acceder a datos privados, lo que garantiza que solo las personas autorizadas puedan obtener acceso a ellos.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Javascript, se pueden seguir los siguientes pasos:

1. Crear una instancia del objeto XMLHttpRequest:
```Javascript
var xhr = new XMLHttpRequest();
```
2. Establecer el método de solicitud y la URL:
```Javascript
xhr.open('GET', 'https://api.example.com/data');
```
3. Crear una cadena con el nombre de usuario y la contraseña en el formato "username:password":
```Javascript
var auth = btoa("username:password");
```
4. Agregar los encabezados necesarios para la autenticación básica:
```Javascript
xhr.setRequestHeader("Authorization", "Basic " + auth);
```
5. Enviar la solicitud al servidor:
```Javascript
xhr.send();
```

Una vez que se completa la solicitud, se puede acceder a la respuesta del servidor a través de la propiedad "responseText" del objeto XMLHttpRequest.

## Profundizando

La autenticación básica es un método de autenticación ampliamente utilizado en el protocolo HTTP. Al enviar una solicitud con autenticación básica, se incluye un encabezado "Authorization" en la solicitud que contiene el nombre de usuario y la contraseña codificados en Base64. Esta codificación no es una forma segura de encriptar los datos, por lo que se recomienda utilizar otros métodos de autenticación si se envían datos sensibles.

Además, cabe mencionar que este método solo cifra la información de inicio de sesión, por lo que se recomienda utilizar una conexión segura (HTTPS) para proteger el resto de la información enviada.

## Ver también

- [Documentación oficial de XMLHttpRequest](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)
- [Ejemplo de autenticación básica en Javascript](https://jsfiddle.net/trevorhreed/dTtyr/)
- [Información sobre otros métodos de autenticación en HTTP](https://tools.ietf.org/html/rfc7617)```