---
title:                "Enviando una solicitud http con autenticación básica."
html_title:           "PHP: Enviando una solicitud http con autenticación básica."
simple_title:         "Enviando una solicitud http con autenticación básica."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP con autenticación básica es una forma segura de proteger las comunicaciones entre un servidor y un cliente. Al proporcionar credenciales de inicio de sesión básicas, se verifica la identidad del usuario y se garantiza que solo aquellos con las credenciales correctas puedan acceder a ciertas páginas web o recursos protegidos.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en PHP, necesitamos utilizar la función `curl_init()` y configurar la autenticación con las opciones `CURLOPT_USERPWD` y `CURLOPT_HTTPAUTH`.

```
<?php
// Inicializar cURL
$ch = curl_init();

// Configurar la url
curl_setopt($ch, CURLOPT_URL, "https://ejemplo.com/recurso-protegido");

// Configurar la autenticación básica
curl_setopt($ch, CURLOPT_USERPWD, "usuario:contraseña");
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

// Ejecutar la solicitud y guardar el resultado
$resultado = curl_exec($ch);

// Cerrar la conexión cURL
curl_close($ch);

// Imprimir el resultado
echo $resultado;
?>
```

La información de autenticación debe proporcionarse en credenciales de usuario y contraseña separadas por dos puntos. En este ejemplo, hemos utilizado la cuenta de usuario "usuario" y la contraseña "contraseña". Luego, se especifica que la autenticación es básica mediante el uso de la constante `CURLAUTH_BASIC`.

## Profundizando

La autenticación básica es uno de los métodos de autenticación más antiguos y menos seguros disponibles para la comunicación en línea. Funciona enviando las credenciales de usuario y contraseña en texto plano a través de la conexión HTTP. Por lo tanto, se recomienda utilizar métodos de autenticación más robustos como HTTPS o OAuth en lugar de la autenticación básica.

Sin embargo, si por alguna razón debes utilizar autenticación básica en tu código PHP, ten en cuenta que es posible que la información de autenticación se guarde en caché a nivel de servidor o de cliente, lo que puede generar problemas de seguridad si se utiliza en conexiones no confiables. Además, si se produce un error, la respuesta del servidor puede ser 401 (no autorizado) o 403 (prohibido), lo que puede ser una pista útil para solucionar problemas de autenticación.

## Ver también

- [Documentación de cURL para PHP](https://www.php.net/manual/es/book.curl.php)
- [Autenticación básica de HTTP en Wikipedia](https://es.wikipedia.org/wiki/Autenticaci%C3%B3n_b%C3%A1sica_de_HTTP)