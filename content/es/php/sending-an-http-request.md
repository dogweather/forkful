---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es el proceso de solicitar datos de un servidor a través de la web. Los programadores lo hacen para interactuar con una API de un servidor remoto, como obtener datos, enviar datos, actualizar datos y borrar datos.

## Cómo hacer:

En PHP, puedes usar la biblioteca cURL para enviar solicitudes HTTP. Aquí tienes un simple ejemplo de cómo enviar una solicitud GET:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL,"http://example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$output = curl_exec($ch);

curl_close ($ch);

var_dump($output);
?>
```
Cuando ejecutes este código, obtendrás una respuesta que será el HTML de la página "http://example.com", lo cual se muestra en modo raw.

## Profundizando

Históricamente, antes de tener cURL en PHP, los desarrolladores dependían de funciones de socket para enviar solicitudes HTTP, lo cual era bastante complicado y propenso a errores.

Además de cURL, también puedes utilizar otras bibliotecas como Guzzle y Requests para PHP, que proporcionan una interfaz de programación más limpia y agradable.

En lo que respecta a los detalles de implementación, todas las solicitudes HTTP están compuestas por un encabezado (Header) y un cuerpo (Body). El cuerpo de una solicitud GET suele estar vacío, mientras que en las solicitudes POST, PUT y DELETE, el cuerpo contiene los datos a enviar.

## Ver también:

1. [Guía de cURL en PHP](http://www.php.net/manual/es/book.curl.php)
2. [Library Guzzle en Github](https://github.com/guzzle/guzzle)
3. [Library Requests para PHP en Github](https://github.com/rmccue/Requests)