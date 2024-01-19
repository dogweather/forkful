---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP implica realizar una petición a un servidor web o API utilizando el protocolo HTTP. Los programadores lo hacen para comunicarse con servicios de la web, obtener o enviar datos.

## Cómo hacerlo:

Enviando una petición GET con curl en Fish Shell es tan fácil como esto:

```Fish Shell
function http_get
    curl $argv[1]
end
```
Ejecútelo con el URL de su elección, así:

```Fish Shell
http_get 'https://jsonplaceholder.typicode.com/posts'
```

Esto va a imprimir la respuesta del servidor en su terminal.

## Inmersión Profunda:

Históricamente, las solicitudes HTTP han sido la manera en que los clientes hablan con los servidores desde que Tim Berners-Lee inventó la web. Hay alternativas a HTTP, como gRPC de Google, pero HTTP y HTTPS son todavía los reyes.

Fish Shell no tiene bibliotecas HTTP integradas como otros lenguajes. En vez de eso, nos apoyamos en la utilidad de línea de comandos curl, incluida en casi todos los sistemas Unix.

## Ver También:

Para una lectura más profunda sobre solicitudes HTTP, recomiendo [La documentación oficial HTTP en MDN](https://developer.mozilla.org/es/docs/Web/HTTP/Overview).

Para aprender más acerca de cómo funciona curl, echa un vistazo a [La documentación de curl](https://curl.haxx.se/docs/manpage.html).

Y por supuesto, puedes consultar [La documentación de Fish Shell](https://fishshell.com/docs/current/index.html) para obtener más detalles sobre cómo funciona y los comandos que puedes usar.