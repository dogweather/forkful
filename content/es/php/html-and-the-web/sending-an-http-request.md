---
date: 2024-01-20 18:00:22.559097-07:00
description: "C\xF3mo hacerlo: **Salida de muestra:**."
lastmod: '2024-04-05T21:54:00.503012-06:00'
model: gpt-4-1106-preview
summary: '**Salida de muestra:**.'
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
```PHP
<?php
// Inicializar cURL
$ch = curl_init();

// Configurar la URL y otras opciones
curl_setopt($ch, CURLOPT_URL, "http://example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// Ejecutar y obtener la respuesta
$response = curl_exec($ch);

// Chequear errores y cerrar
if(curl_errno($ch)) {
    echo 'Error: ' . curl_error($ch);
} else {
    // Mostrar la respuesta (para fines de ejemplo)
    echo $response;
}

// Cerrar cURL
curl_close($ch);
?>
```

**Salida de muestra:**

```HTML
<!doctype html>
<html>
<head>
    <title>Ejemplo</title>
</head>
<body>
    <p>Este es un ejemplo de una página web a la que se accedió mediante una solicitud HTTP de PHP.</p>
</body>
</html>
```

## Profundización:
Enviar solicitudes HTTP es una característica universal de la web. Originalmente, PHP usaba funciones como `fopen()` y `file_get_contents()` para obtener datos de URLs externas, pero estas opciones son limitadas y no pueden manejar todas las situaciones, en especial cuando se necesitan métodos HTTP como POST o PUT, o manejo de cabeceras y cookies.

cURL, o Client URL Library, es más sofisticado. Apareció en 1997 y desde entonces se ha convertido en una opción estándar para manejar solicitudes HTTP en PHP porque es poderoso y flexible. cURL admite autenticación, métodos HTTP personalizables, manejo de cookies, y mucho más.

Además de cURL, existen múltiples bibliotecas de terceros que simplifican aún más el envío de solicitudes como Guzzle. Pero cURL sigue siendo una herramienta fundamental y directamente disponible sin necesidad de instalar extras.

La implementación de una solicitud HTTP puede variar en complejidad dependiendo de la necesidad del programador, pero en su forma más básica, como se muestra en el ejemplo anterior, se puede realizar en pocos pasos utilizando cURL.

## Ver También:
- Documentación oficial de cURL para PHP: [https://www.php.net/manual/es/book.curl.php](https://www.php.net/manual/es/book.curl.php)
- Guía de HTTP de Mozilla Developer Network (MDN) para entender mejor las solicitudes HTTP y sus características: [https://developer.mozilla.org/es/docs/Web/HTTP](https://developer.mozilla.org/es/docs/Web/HTTP)
- Guzzle, un cliente HTTP PHP que simplifica el envío de solicitudes: [http://docs.guzzlephp.org/en/stable/](http://docs.guzzlephp.org/en/stable/)
- Tutorial sobre solicitudes HTTP con file_get_contents: [https://www.php.net/manual/es/function.file-get-contents.php](https://www.php.net/manual/es/function.file-get-contents.php)
