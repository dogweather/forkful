---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Bash: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma comúnmente utilizada para acceder a recursos protegidos en servidores web. Esto permite a los usuarios autenticarse con un nombre de usuario y contraseña para acceder a contenido restringido o realizar acciones en una aplicación web.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Bash, podemos utilizar el comando "curl" seguido de la bandera "-u" y especificando el nombre de usuario y contraseña en el siguiente formato: usuario:contraseña. Por ejemplo:

```
curl -u usuario:contraseña https://ejemplo.com/recurso-protegido
```

Al presionar Enter, se enviará una solicitud HTTP al servidor con los datos de autenticación incluidos en la cabecera de la solicitud. Si la autenticación es correcta, se devolverá la respuesta del servidor, de lo contrario, se mostrará un código de error.

## Profundizando

En una solicitud HTTP con autenticación básica, el nombre de usuario y contraseña se codifican en base64 antes de ser enviados al servidor. Esto significa que no se están encriptando los datos, por lo que no se considera una forma segura de autenticación. Para una mayor seguridad, se recomienda utilizar otros métodos de autenticación, como HTTPS o OAuth.

## Ver también

- [Documentación oficial de curl](https://curl.se/docs/manpage.html)
- [Tutorial sobre solicitudes HTTP en Bash](https://www.baeldung.com/http-request-bash)
- [Artículo sobre autenticación básica en solicitudes HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)