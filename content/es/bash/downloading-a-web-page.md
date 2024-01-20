---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Descarga de Páginas Web Con Bash

## ¿Qué y Por Qué?
La descarga de una página web es básicamente adquirir todo su contenido HTML. Los programadores lo hacen para analizar su estructura, extraer información relevante o probar su rendimiento.

## ¿Cómo se Hace?
Para descargar una página web usando Bash, usamos el comando `curl` o `wget`. Aquí te mostramos cómo se hace:

```bash
# Utilizando curl
curl https://www.example.com -o example.html

# Utilizando wget
wget https://www.example.com
```

La salida será el contenido HTML de la página web descargada.

```bash
# Ejemplo de salida
<!DOCTYPE html>
<html>
<body>
    <h1>Bienvenido a Example.com</h1>
</body>
</html>
```

## Un vistazo más Profundo
El comando `curl` se lanzó en 1997 y `wget` en 1996, ambos como herramientas de transferencia de datos. Existen varias alternativas como 'lynx', 'fetch' o 'HTTPie'. Sus detalles de implementación varían, pero todos tienen en común el envío de una solicitud HTTP GET al servidor y la recepción de los datos de respuesta.

`curl` es más práctico, ofrece más opciones y también puede manejar cookies. Por otro lado, `wget` es más fácil de usar y puede descargar automáticamente todos los recursos vinculados en una página web.

## Ver También
Para más detalles, puedes revisar el manual oficial de `curl`: https://curl.se/docs/manpage.html, y `wget` en: https://www.gnu.org/software/wget/manual/wget.html. También puedes compararlos en profundidad en: https://daniel.haxx.se/docs/curl-vs-wget.html.