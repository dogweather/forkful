---
title:                "Descargando una página web"
aliases:
- /es/fish-shell/downloading-a-web-page.md
date:                  2024-01-20T17:43:58.209939-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Descargar una página web es traer su contenido, usualmente HTML, a tu disco local. Los programadores necesitan hacer esto para análisis de datos, pruebas de aplicaciones web o simplemente para guardar una copia.

## Cómo hacerlo:
Para descargar una página web en Fish Shell, puedes usar herramientas como `curl` o `wget`. Aquí tienes un ejemplo con `curl`:

```Fish Shell
curl -o nombredelarchivo.html http://ejemplo.com
```

Esto guarda la página de `http://ejemplo.com` en un archivo llamado `nombredelarchivo.html`.

Con `wget` sería así:

```Fish Shell
wget -O nombredelarchivo.html http://ejemplo.com
```

La `-O` (mayúscula) le indica a `wget` el nombre del archivo de salida.

Salida de ejemplo para `curl`:

```Fish Shell
% curl -o nombredelarchivo.html http://ejemplo.com
% cat nombredelarchivo.html
<!DOCTYPE html>...
```

Muestras el contenido descargado con `cat`.

## Profundización:

A lo largo de la historia, la descarga de páginas web ha sido fundamental para la indexación por motores de búsqueda y el análisis de contenido, permitiendo servicios como Google. `curl` y `wget` son herramientas de línea de comandos que han existido durante décadas; `curl` comenzó en 1997 y `wget` en 1996. Mientras que `curl` puede hacer muchas cosas además de descargar archivos (como enviar datos con POST, trabajar con APIs, etc.), `wget` se enfoca más en descargar contenidos desde Internet de manera recursiva, ofreciendo la capacidad de descargar sitios completos.

Desde el punto de vista de implementación, al usar estas herramientas en un script de Fish Shell, es posible añadirles opciones para personalizar las solicitudes HTTP (por ejemplo, añadiendo encabezados o autenticación) y gestionar errores de red o HTTP efectivamente.

Si bien estas herramientas son suficientes para tareas básicas, para un raspado (scraping) más avanzado podrías necesitar herramientas más especializadas en Python como `BeautifulSoup` o `Scrapy`.

## Ver También:

- Documentación de `curl`: https://curl.se/docs/
- Guía de `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Tutorial de `BeautifulSoup`: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Página oficial de `Scrapy`: https://scrapy.org

Estas fuentes te darán una mejor idea sobre cómo estas herramientas pueden ser utilizadas y qué más puedes hacer con ellas.
