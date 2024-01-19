---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Descargar una página web significa guardar una copia del contenido a nivel de código fuente en tu dispositivo. Los programadores suelen hacerlo para estudiar el código, depurar problemas o para interfaces de programación de aplicaciones (APIs) de raspado.

## ¿Cómo se hace?

Descargaremos una página web usando `curl` en la Terminal Fish. Aquí tienes un ejemplo sencillo:

```Fish Shell
curl -O https://example.com
```

El código anterior descargará el contenido HTML de la página `example.com` y lo conservará en un archivo llamado `index.html`. Por defecto, `curl` utilizará el nombre del archivo sugerido por la URL.

Para guardar la página con un nombre específico, usa `-o` seguido por el nombre del archivo:

```Fish Shell
curl -o mipagina.html https://example.com
```

## Profundizando

Históricamente, `curl` es una herramienta poderosa y flexible para transferir datos desde o hacia una red, con soporte para una gran cantidad de protocolos. Fue lanzado por primera vez en 1997 y su nombre significa "Client URL", lo que evidencia su objetivo principal.

Existen alternativas a `curl`, como `wget`, que funciona de manera similar pero tiene algunas diferencias, como el seguimiento de enlaces en páginas HTML de manera recursiva, algo que `curl` no hace por defecto.

En cuanto a implementación, `curl` realiza una solicitud GET HTTP para descargar la página. Podrías configurarlo para usar otros métodos de solicitud como POST o HEAD si lo necesitaras.

## Ver También

Para un análisis más detallado de `curl` y sus muchas características, consulta la [página de manual de curl](https://curl.se/docs/manpage.html).

Para una discusión sobre `curl` vs. `wget`, visita este [enlace en Stack Overflow](https://stackoverflow.com/questions/333847/curl-vs-wget-what-is-the-difference).