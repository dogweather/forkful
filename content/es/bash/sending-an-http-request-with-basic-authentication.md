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

## ¿Qué y por qué?
En la programación, a veces es necesario enviar una solicitud HTTP con autenticación básica, lo que significa que debemos proporcionar un nombre de usuario y contraseña para acceder a una página o servicio. Los programadores hacen esto para asegurarse de que solo usuarios autorizados puedan acceder a la información protegida.

## Cómo:
Para enviar una solicitud HTTP con autenticación básica usando Bash, podemos utilizar el siguiente comando:
```Bash
curl -u username:password URL
```

Esto enviará una solicitud GET a la URL especificada con las credenciales de autenticación incluidas en la cabecera. Si la autenticación es exitosa, recibiremos una respuesta del servidor.

## Profundizando:
En el pasado, la autenticación básica era el único método de autenticación disponible en HTTP. Sin embargo, ahora se recomienda el uso de otros métodos de autenticación más seguros, como OAuth. Además, al enviar una solicitud con autenticación básica, las credenciales se envían en texto plano y pueden ser interceptadas fácilmente, lo que hace que este método sea vulnerable a ataques de seguridad.

El comando curl que usamos en el ejemplo anterior también permite especificar otros parámetros, como el tipo de solicitud, los encabezados de la solicitud y el cuerpo de la misma. Esto nos da más control sobre cómo se envía la solicitud y qué información se incluye en ella.

## Ver también:
- [Documentación oficial de cURL](https://curl.haxx.se/docs/)
- [Tutorial de autenticación básica en HTTP](https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-http)
- [Alternativas más seguras a la autenticación básica en HTTP](https://www.owasp.org/index.php/Basic_authentication)