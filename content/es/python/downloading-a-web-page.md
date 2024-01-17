---
title:                "Descargando una página web"
html_title:           "Python: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web simplemente significa guardar una copia de la página en tu computadora. Los programadores suelen hacer esto para analizar el código fuente de la página o extraer datos estructurados de ella.

## ¡Cómo hacerlo!

Para descargar una página web en Python, puedes utilizar el módulo incorporado `urllib.request` y la función `urlretrieve()`. En el siguiente bloque de código, usaremos esta función para descargar la página principal de Wikipedia y guardarla en nuestro directorio actual como "wikipedia.html":

```Python 
from urllib.request import urlretrieve

url = "https://es.wikipedia.org/wiki/Wikipedia:Portada"
urlretrieve(url, "wikipedia.html")
```

Una vez que se ejecute el código, la página de Wikipedia se guardará en la misma carpeta que tu archivo de Python. ¡Fácil, ¿verdad?

## Una visión más profunda

Descargar páginas web ha sido una práctica común entre los programadores desde los primeros días de Internet. Antes de la existencia de bibliotecas y módulos especializados, los programadores solían escribir su propio código para descargar y analizar páginas web.

Además de la función `urlretrieve()`, también puedes usar la biblioteca `requests` de Python para descargar páginas web. Esta biblioteca es más fácil de usar y proporciona una API más consistente para hacer solicitudes HTTP.

También existen herramientas como wget y curl que pueden descargar páginas web rápidamente desde la línea de comandos. Sin embargo, no ofrecen las poderosas capacidades de procesamiento y análisis de Python.

## Vea también

Para obtener más información sobre cómo descargar páginas web en Python, revisa la documentación oficial de Python sobre `urllib.request` y `requests`. También puedes explorar las bibliotecas de terceros como Beautiful Soup y Scrapy para analizar páginas web. ¡Feliz descarga!