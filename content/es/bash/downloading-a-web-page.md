---
title:                "Descargar una página web"
html_title:           "Bash: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La descarga de una página web es el proceso de obtener una copia de la página desde un servidor de internet a su dispositivo local. Los programadores a menudo descargan páginas web para obtener datos que luego pueden utilizar en sus aplicaciones o para realizar pruebas y debugging.

## Cómo:

Usando el lenguaje de programación Bash, puedes descargar una página web utilizando el comando `wget`. Por ejemplo, si queremos descargar la página principal de Google en nuestro directorio actual, podemos usar el comando:

```
Bash wget https://www.google.com
```

Esto descargará la página y mostrará el progreso en la terminal. También puedes utilizar otros parámetros como `-r` para descargar recursivamente todas las páginas vinculadas, o `-O` para especificar un nombre de archivo para guardar la página descargada.

## Profundizando:

La descarga de páginas web se ha vuelto cada vez más común con el auge de la programación web y la necesidad de obtener datos de internet. Aparte de usar Bash, hay varias herramientas y librerías disponibles en otros lenguajes de programación para descargar páginas web, como `urllib` en Python o `curl` en C. También hay APIs disponibles para hacer solicitudes y descargar datos de manera más estructurada.

## Véase también:

- [Documentación de `wget`](https://www.gnu.org/software/wget/)
- [Documentación de `urllib` en Python](https://docs.python.org/3/howto/urllib2.html)
- [Documentación de `curl`](https://curl.haxx.se/docs/manpage.html)