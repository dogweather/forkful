---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El envío de un pedido HTTP con autenticación básica es un proceso en el que un cliente solicita un recurso protegido al proporcionar credenciales. Los programadores lo hacen para acceder a recursos de manera segura en la web, evitando accesos no autorizados.

## ¿Cómo hacerlo?
La Fish Shell facilita el envío de solicitudes HTTP con autenticación básica. Veamos cómo se hace:

```Fish Shell
set cURLUser 'usuario'
set cURLPassword 'contraseña'
set URL 'https://unrecursoseguro.com'

curl -u $cURLUser:$cURLPassword $URL
```
Este comando intentará recuperar los recursos de la URL ingresada utilizando las credenciales especificadas.

## Profundizando
Históricamente, la autenticación básica ha sido una de las formas más simples de implementar la autenticación en HTTP. Sin embargo, seguramente habrás escuchado de alternativas más seguras, como OAuth y JWT, ya que la autenticación básica expone las contraseñas en texto plano sobre el tráfico de red.

En Fish Shell, el envío de solicitudes HTTP con autenticación básica se implementa utilizando el comando `curl`. Aquí, las credenciales se pasan mediante el argumento `-u`, seguido del nombre de usuario y la contraseña separados por dos puntos.

## Ver también
Para aprender más sobre el uso de Fish Shell y el envío de solicitudes HTTP con autenticación básica, puedes visitar las siguientes fuentes:

- [Guía de cURL para solicitudes HTTP](https://ec.haxx.se/http/http-auth)