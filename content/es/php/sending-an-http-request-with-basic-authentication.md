---
title:                "PHP: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar solicitudes HTTP con autenticación básica es una forma segura y sencilla de acceder a información protegida en un servidor. Con esta técnica, se puede garantizar que solo los usuarios autorizados puedan acceder a recursos específicos en una aplicación web.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en PHP, primero es necesario crear un objeto `curl` con la URL del recurso al que se desea acceder. Luego, se puede usar la función `curl_setopt` para configurar las opciones de la solicitud, incluyendo el tipo de autenticación y las credenciales del usuario.

A continuación se muestra un ejemplo de cómo enviar una solicitud GET con autenticación básica a una API:

```PHP
// Crear objeto curl
$curl = curl_init("https://api.example.com/users/123");

// Configurar opciones de la solicitud
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "usuario:contraseña");

// Ejecutar la solicitud y obtener la respuesta
$response = curl_exec($curl);

// Imprimir la respuesta
echo $response;

// Cerrar conexión curl
curl_close($curl);
```
La respuesta de esta solicitud debería ser una representación en formato JSON del usuario con ID 123, siempre y cuando se proporcione la autenticación correcta.

## Profundizando

La autenticación básica es considerada como un mecanismo relativamente inseguro de autenticación, ya que las credenciales se envían como texto plano en la solicitud. Además, no ofrece una forma de renovar las credenciales después de cierto tiempo, lo que puede ser un problema en aplicaciones de larga duración.

Para aumentar la seguridad de las solicitudes con autenticación básica, se pueden tomar medidas adicionales como usar HTTPS y almacenar las credenciales encriptadas en lugar de en texto plano. También es importante recordar que las credenciales no deben ser almacenadas en código fuente o compartirse de manera insegura.

## Véase también

- [Tutorial de autenticación básica con PHP y cURL](https://www.codeofaninja.com/2013/04/basic-http-authentication-php-curl.html)
- [Documentación oficial de cURL](https://www.php.net/manual/es/book.curl.php)
- [Artículo sobre seguridad en autenticación básica](https://dzone.com/articles/how-bad-basic-authentication)