---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP con autenticación básica es una forma eficaz de permitir un acceso seguro a un recurso a través de la web. Los programadores lo hacen para validar la identidad del usuario antes de concederle el acceso a los recursos.

## Cómo hacerlo:

Podemos usar la biblioteca cURL en PHP para enviar una solicitud HTTP con autenticación básica. Aquí tienes un ejemplo básico:

```PHP
<?php

$url = "https://your-url.com"; 

$ch = curl_init(); 

curl_setopt($ch, CURLOPT_URL, $url); 

curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1); 

curl_setopt($ch, CURLOPT_USERPWD, "username:password"); 

$result = curl_exec($ch);

if(curl_errno($ch)){
    echo 'Curl error: ' . curl_error($ch);
}

// Imprimir el resultado
echo $result;

curl_close($ch);
?>
```

Este código enviará una solicitud HTTP GET a la URL especificada con la autenticación básica.

## Deep Dive:

En cuanto al contexto histórico, la autenticación básica ha sido parte de HTTP desde los primeros días de internet. Sin embargo, no cifra tus credenciales, por lo que en general se combina con SSL.

Además de cURL, también puedes usar otras bibliotecas como Guzzle y Requests para enviar solicitudes HTTP con autenticación básica. Cada una tiene sus ventajas y características únicas.

La implementación de la autenticación básica por medio de una solicitud HTTP es simple: se envía un encabezado de 'Authorization' con el valor 'Basic' seguido por las credenciales de usuario y contraseña codificadas en Base64.

## Ver también:

1. Documentación de PHP cURL: [https://www.php.net/manual/es/book.curl.php](https://www.php.net/manual/es/book.curl.php)
2. Biblioteca PHP Guzzle: [http://docs.guzzlephp.org/en/stable/](http://docs.guzzlephp.org/en/stable/)
3. Library PHP Requests: [https://requests.ryanmccue.info/](https://requests.ryanmccue.info/)
4. Autenticación básica en HTTP: [https://developer.mozilla.org/es/docs/Web/HTTP/Authentication](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)