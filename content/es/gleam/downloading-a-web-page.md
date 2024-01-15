---
title:                "Descargando una página web"
html_title:           "Gleam: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Si eres desarrollador web, es posible que necesites descargar una página web para hacer pruebas o realizar cambios en ella. Con Gleam, puedes automatizar este proceso y ahorrar tiempo y esfuerzo.

## Cómo hacerlo

Para descargar una página web con Gleam, sigue estos sencillos pasos:

1. Importa el módulo `http` de Gleam.
2. Crea una función `download_webpage` que tome la URL de la página como argumento.
3. Hace una solicitud GET a la URL usando la función `http.get`.

`` `Gleam
import http

fn download_webpage(url) {
    http.get(url)
} `` `

4. Ahora puedes llamar a la función con la URL de la página que quieres descargar.

`` `Gleam
download_webpage("https://www.gleam.dev")`` `

5. Para guardar el contenido de la página en un archivo, puedes utilizar el módulo `file` de Gleam y la función `file.write`.

`` `Gleam
import http
import file

fn download_webpage(url) {
    http.get(url)
}

let response = download_webpage("https://www.gleam.dev")
file.write("webpage.html", response.body)`` `

¡Listo! Ahora tienes el contenido de la página web descargado y guardado en un archivo.

## Profundizando

Si quieres personalizar tu proceso de descarga de páginas web, puedes utilizar las funciones y métodos adicionales del módulo `http` de Gleam. Por ejemplo, puedes especificar encabezados personalizados en tu solicitud GET o descargar imágenes y otros recursos incrustados en la página web.

## Ver también

- Documentación oficial de Gleam sobre el módulo `http`: https://gleam.run/modules/http.html
- Tutorial sobre cómo descargar una página web con Gleam: https://www.gleam.dev/tutorials/web_download/