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

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es una forma de comunicarse con un servidor web protegido mediante un usuario y contraseña. Los programadores lo hacen para verificar la identidad del usuario y acceder a recursos restringidos.

## Cómo:
```
// Ejemplo de código en Javascript para enviar una solicitud HTTP con autenticación básica

// Crear un objeto XMLHttpRequest
const request = new XMLHttpRequest();

// Especificar el método HTTP y la URL del servidor
request.open('GET', 'https://www.ejemplo.com/recurso-protegido');

// Agregar los encabezados de autenticación básica
request.setRequestHeader('Authorization', 'Basic usuario:contraseña');

// Enviar la solicitud
request.send();

// Obtener la respuesta del servidor
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    // Hacer algo con los datos recibidos
    console.log(this.responseText);
  }
}
```

La salida de este código podría ser una respuesta en formato JSON, XML o simplemente un mensaje de éxito o error, dependiendo de la implementación del servidor.

## Profundizando:
La autenticación básica es un método de autenticación de HTTP que ha existido desde los inicios de la web en 1999. Aunque sigue siendo utilizado, se considera menos seguro que otros métodos más recientes como OAuth o JWT (JSON Web Tokens). Sin embargo, sigue siendo una opción común para aplicaciones internas o en casos donde la seguridad no sea una preocupación tan importante. Además, es fácil de implementar y no requiere librerías adicionales.

## Ver también:
- [Introducción a la autenticación HTTP básica](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [Alternativas a la autenticación básica](https://www.moesif.com/blog/technical/api-authentication-methods/)
- [Especificación de autenticación básica en HTTP](https://tools.ietf.org/html/rfc2617)