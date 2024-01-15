---
title:                "Enviando una petición http"
html_title:           "PHP: Enviando una petición http"
simple_title:         "Enviando una petición http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar una solicitud HTTP es importante en la programación web, ya que permite a las aplicaciones comunicarse con servidores y obtener información. Es necesario para obtener datos, enviar formularios, autenticar usuarios y más.

## Cómo hacerlo
Para enviar una solicitud HTTP en PHP, se utiliza la función `curl_init()`. A continuación, se establecen las opciones de la solicitud, como la URL de destino y el método HTTP utilizado. Luego se ejecuta la solicitud con `curl_exec()` y se obtiene la respuesta del servidor.

```PHP
<?php
// Iniciar una nueva solicitud
$ch = curl_init();

// Configurar URL y otros datos de la solicitud
curl_setopt($ch, CURLOPT_URL, "https://www.ejemplo.com/");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

// Establecer el método HTTP utilizado
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "GET");

// Ejecutar la solicitud y obtener la respuesta
$response = curl_exec($ch);

// Cerrar la solicitud
curl_close($ch);

// Imprimir la respuesta
echo $response;
?>
```

La respuesta será una cadena con el contenido de la página solicitada. Esto se puede utilizar para mostrar datos en una aplicación, almacenarlos en una base de datos o cualquier otra tarea que se necesite.

## Profundizando
La función `curl_init()` permite establecer más opciones, como encabezados de solicitud, autenticación, manejo de errores y más. Se recomienda revisar la documentación oficial de PHP para obtener más información sobre las opciones disponibles y cómo utilizarlas correctamente en diferentes escenarios.

## Ver también
- [Documentación oficial de curl en PHP](https://www.php.net/manual/es/book.curl.php)
- [Tutorial de curl en Tuts+](https://code.tutsplus.com/es/tutorials/http-requests-in-php-using-curl--net-16275)