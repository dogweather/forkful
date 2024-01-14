---
title:                "PHP: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué descargar una página web es útil

Descargar páginas web puede ser útil por varias razones, como por ejemplo guardar una copia local de un sitio importante, acceder a contenido que no está disponible offline, o simplemente para realizar pruebas de desarrollo.

## Cómo descargar una página web en PHP

Para descargar una página web en PHP, podemos utilizar la librería cURL. A continuación, se muestra un ejemplo de código que descarga una página y muestra su contenido en la consola:

```PHP
<?php
// Creamos una instancia de cURL
$ch = curl_init();

// Especificamos la URL a descargar
curl_setopt($ch, CURLOPT_URL, "https://www.ejemplo.com");

// Indicamos que queremos guardar el resultado en una variable
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

// Ejecutamos la solicitud
$page = curl_exec($ch);

// Cerramos la conexión
curl_close($ch);

// Mostramos el resultado en la consola
echo $page;
?>
```

El resultado de este código será el contenido HTML de la página descargada.

## Profundizando en la descarga de páginas web

La librería cURL ofrece una gran cantidad de opciones y configuraciones que nos permiten personalizar aún más nuestras solicitudes. Por ejemplo, podemos especificar una serie de cabeceras en la solicitud para simular un dispositivo móvil y obtener una versión de la página adaptada a dispositivos móviles. También podemos utilizar autenticación HTTP para acceder a contenido restringido.

Otro aspecto importante a considerar es la optimización de la descarga de páginas web, ya que una mala implementación puede llevar a un consumo excesivo de recursos del servidor y a un tiempo de respuesta lento. Es recomendable utilizar técnicas de caching y compresión para mejorar el rendimiento.

## Ver también

- [Documentación de cURL en PHP](https://www.php.net/manual/es/book.curl.php)
- [Ejemplos de cURL en PHP](https://github.com/curl/docs/blob/master/examples/php/example.php)