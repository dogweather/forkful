---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Descargar una página web implica guardar una copia de una página específica en tu dispositivo local. Los programadores lo hacen para analizar la estructura de la página, extraer datos, probar la velocidad de descarga y otras diversas razones funcionalistas.

## Como hacerlo:

Aquí te mostramos cómo descargar una página web usando PHP. Este es un ejemplo simple que utilizará la función file_get_contents.

```PHP
<?php
$paginaWeb = file_get_contents('https://www.example.com');

echo $paginaWeb;
?>
```

En este ejemplo, si ejecutas este código, verás todo el código HTML de "https://www.example.com" impreso en tu pantalla.

## Buceando más profundo

El método `file_get_contents` en PHP es una solución fácil y rápida para descargar una página web. Sin embargo, fue presentado en PHP 4.3.0, por lo que si trabajas con versiones más antiguas de PHP, este método no funcionará.

Algunas alternativas podrían ser el uso de `cURL` o `fopen/fread`, aunque estos métodos requieren un poco más de configuración y codificación.

Recuerda que cuando descargas una página web con PHP, en realidad estás realizando una petición GET al servidor del sitio web. Asegúrate de que tu uso de este método se adhiere a las políticas de robots.txt del sitio y a las leyes de protección de datos locales.

## Ver También

1. PHP Official Documentation - file_get_contents: http://php.net/manual/es/function.file-get-contents.php
2. Tutorial sobre cURL con PHP - StackOverflow: https://stackoverflow.com/questions/5647461/how-do-i-send-a-post-request-with-php
3. Políticas de robots.txt - Google Webmasters: https://developers.google.com/search/docs/advanced/robots/robots_txt
4. General Data Protection Regulation (GDPR): https://gdpr-info.eu/