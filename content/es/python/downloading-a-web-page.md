---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Descargando una Página Web con Python - Una Guía Sencilla

## ¿Qué y Por Qué?

Descargar una página web es básicamente obtener el código fuente HTML de la misma. Los programadores suelen hacerlo para realizar web scraping o pruebas automatizadas.

## Cómo hacerlo:

Para hacer esto en Python, uno de los módulos más útiles es `requests`. Aquí está la forma más simple de cómo puede hacerlo:

```Python
import requests

url = "http://www.google.com"
respuesta = requests.get(url)

print(respuesta.text)
```

Esto descargará el HTML de la página de inicio de Google e imprimirá el resultado en la consola.

## Inmersión Profunda

Historia: El paquete `requests` de Python se introdujo en 2011 como una forma mejorada y más fácil para los programadores de Python para trabajar con solicitudes HTTP.

Alternativas: Además de `requests`, también puedes elegir `urllib` que es una biblioteca incorporada en Python. Sin embargo, `requests` es a menudo preferido por su interfaz de usuario amigable.

Detalles de implementación: Cuando llamas a `requests.get()`, estás enviando una solicitud HTTP GET al servidor y recibes una respuesta que contiene el HTML de la página web.

## Ver También

Si deseas explorar más sobre web scraping o el paquete `requests`, aquí tienes algunos enlaces útiles:

- Para `requests`: [Documentación Oficial de Requests](https://docs.python-requests.org/en/latest/)
- Para web scraping: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
- Para un tutorial más avanzado: [Web Scraping con Python: Guía Completa](https://realpython.com/python-web-scraping-practical-introduction/)