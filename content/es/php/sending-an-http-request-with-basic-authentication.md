---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:02:24.100113-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica es el proceso de hacer un pedido a un servidor incluyendo un usuario y contraseña en la cabecera de la solicitud. Los programadores lo hacen para acceder a recursos protegidos que requieren una verificación de identidad simple.

## Cómo hacerlo:

Para enviar una solicitud con autenticación básica en PHP, únicamente necesitas unas pocas líneas de código. Aquí tienes un ejemplo usando `curl`:

```PHP
<?php
$usuario = 'miUsuario';
$contrasena = 'miContraseña';

$url = 'https://api.ejemplo.com/datos';
$credenciales = base64_encode("$usuario:$contrasena");

$ch = curl_init($url);

curl_setopt($ch, CURLOPT_HTTPHEADER, ["Authorization: Basic $credenciales"]);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$respuesta = curl_exec($ch);
curl_close($ch);

echo $respuesta;
?>
```

Y esto es lo que obtendrás como salida, claro, dependiendo del recurso al que accedas:

```
{"mensaje":"¡Acceso concedido! Aquí están tus datos..."}
```

## Análisis Detallado:

Historia: La autenticación básica HTTP existe desde los primeros días de la web. A pesar de su simplicidad y fallos de seguridad, sigue usándose debido a su facilidad de implementación.

Alternativas: Dada su vulnerabilidad (envía credenciales en texto base64, que es fácilmente decodificable), hoy en día se prefiere utilizar otros métodos más seguros como OAuth, tokens de API o autenticación vía JWT (JSON Web Tokens).

Detalles de implementación: El uso de `curl` en PHP es común para hacer solicitudes HTTP porque ofrece gran flexibilidad. Es importante manejar errores y asegurarte de que `curl_exec()` no devuelva `false`, lo que indicaría un fallo en la solicitud.

## Ver También:

Para más detalles sobre la autenticización básica HTTP y sus alternativas más seguras, consulta los siguientes enlaces:

- [Autenticación HTTP básica en php.net](https://www.php.net/manual/es/features.http-auth.php)
- [Documentación Curl en PHP](https://www.php.net/manual/es/book.curl.php)
- [JWT.io](https://jwt.io/)
- [Introducción a OAuth 2.0](https://oauth.net/2/)
