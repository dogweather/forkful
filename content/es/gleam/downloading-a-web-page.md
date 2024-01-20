---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Descargar una página web es obtener su código fuente para manipularla o analizarla. Los programadores lo hacen para recopilar datos, automatizar pruebas y tareas, o para crear versiones sin conexión de las páginas.

## Cómo hacerlo:

Aquí se muestra un ejemplo sencillo en Gleam de cómo descargar una página web.

``` Gleam
import gleam/httpc

fn download_page(url: String) {
  let Ok(body) = httpc.get(url)
  Console.display(body)
}

download_page("https://example.com")
```

Al ejecutar este código, verás que la respuesta completa del servidor se imprime en tu consola.

## Inmersión profunda:

Históricamente, las bibliotecas de hardware gestionaban la descarga de páginas web. Pero, con el tiempo, los lenguajes de programación como Gleam introdujeron módulos internos para manejar estas tareas. 

Existen otras formas de descargar páginas web, como el raspado web o los sockets de red, pero usar una biblioteca http es una opción mucho más simple y directa para la mayoría de los casos. 

El método `httpc.get` en Gleam realiza una petición GET al URL proporcionado y devuelve la respuesta del servidor. Lo que realmente sucede detrás de escena implica abrir una conexión al servidor, enviar la petición, recibir la respuesta y finalmente cerrar la conexión.

## Ver también:

Para saber más acerca de estas temáticas, puedes consultar los siguientes enlaces:

- Documentación oficial de Gleam: [Gleam HTTP API](https://hexdocs.pm/gleam_httpc/gleam/httpc.html)
- Documentación de Mozilla sobre HTTP: [An overview of HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)