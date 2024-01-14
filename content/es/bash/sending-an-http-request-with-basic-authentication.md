---
title:                "Bash: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

El envío de una solicitud HTTP con autenticación básica es una forma segura de comunicarse con servidores web y acceder a recursos protegidos. A menudo se utiliza para acceder a API de terceros o para realizar acciones que requieren un alto nivel de seguridad, como la autenticación de usuarios en una aplicación web.

## Cómo hacerlo

La forma más común de enviar una solicitud HTTP con autenticación básica en Bash es a través del comando `curl`. Este comando se utiliza para transferir datos desde o hacia un servidor utilizando uno de los muchos protocolos soportados, incluyendo HTTP.

Para enviar una solicitud HTTP con autenticación básica, primero debemos obtener una clave de autenticación, que generalmente se proporciona junto con un nombre de usuario y una contraseña. Luego, utilizando el comando `curl`, podemos incluir esta clave de autenticación en la solicitud utilizando la opción `-u`. Por ejemplo:

```bash
curl -u usuario:contraseña https://miapp.com/api/datos
```

Esta solicitud incluirá la clave de autenticación en la cabecera de la solicitud, lo que permitirá que el servidor web valide nuestro acceso.

Pero, ¿cómo sabemos si la solicitud fue exitosa? Podemos utilizar la opción `-i` de `curl` para incluir las cabeceras de respuesta en la salida. Por ejemplo:

```bash
curl -i -u usuario:contraseña https://miapp.com/api/datos

HTTP/1.1 200 OK
Date: Mon, 01 Mar 2021 12:00:00 GMT
Server: Apache/2.4.6 (CentOS)
Content-Length: 43
Content-Type: application/json; charset=UTF-8

{"id": 1, "nombre": "Juan", "apellido": "Pérez"}
```

Esto nos permitirá ver la respuesta del servidor y asegurarnos de que nuestra autenticación fue exitosa.

## Profundizando

Cuando enviamos una solicitud HTTP con autenticación básica, lo que estamos haciendo es enviar nuestras credenciales (nombre de usuario y contraseña) en texto plano a través de la red. Esto puede ser un riesgo para nuestra seguridad, por lo que es importante utilizar conexiones seguras (HTTPS) al enviar solicitudes HTTP con autenticación básica.

Además, es importante asegurarse de que nuestras credenciales sean almacenadas de manera segura en nuestra aplicación, ya que si alguien más las obtiene, podría acceder a nuestros recursos protegidos.

## Ver también

Para obtener más información sobre cómo utilizar el comando `curl` para enviar solicitudes HTTP con autenticación básica en Bash, recomendamos revisar los siguientes recursos:

- [Documentación oficial de `curl`](https://curl.se/docs/manpage.html)
- [Tutorial de Envío de solicitudes HTTP desde la línea de comando con `curl`](https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-access-remote-services)
- [Tutorial de Autenticación HTTP básica con `curl`](https://www.baeldung.com/curl-http-basic-authentication)