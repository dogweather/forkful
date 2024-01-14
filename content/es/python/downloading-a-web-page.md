---
title:                "Python: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué descargar una página web?

Descargar una página web es una habilidad útil en el mundo de la programación ya que permite obtener información específica de una página de manera rápida y eficiente. Esto puede ser útil para la extracción de datos o para la automatización de tareas.

## Cómo hacerlo

Para descargar una página web en Python, se puede utilizar la librería `urllib` y su función `urlopen()`. A continuación se muestra un ejemplo de cómo descargar la página de inicio de Google y guardar el código HTML en un archivo:

```Python
from urllib.request import urlopen

# Descargar la página de inicio de Google
url = "https://www.google.com"
pagina = urlopen(url)

# Leer y guardar el código HTML en un archivo
html = pagina.read()
with open("google.html", "wb") as archivo:
    archivo.write(html)
```

El resultado de este código será un archivo llamado "google.html" que contiene todo el código HTML de la página de inicio de Google. A partir de ahí, se pueden utilizar otras técnicas para extraer la información deseada.

## Profundizando

La librería `urllib` ofrece diferentes funcionalidades para el manejo de paginas web y sus contenidos. Por ejemplo, la función `urlopen()` tiene varios parámetros opcionales que permiten ajustar la forma en que se realiza la descarga, como por ejemplo especificar un agente de usuario o un tiempo de espera. Además, también es posible enviar formularios o realizar autenticación HTTP con esta librería.

Otra opción para descargar páginas web en Python es utilizar la librería `requests`, que ofrece una interfaz más sencilla y amigable para realizar peticiones HTTP.

¡Explora estas librerías y encuentra la que mejor se adapte a tus necesidades para descargar páginas web en tus proyectos!

## Ver también

- Documentación de la librería `urllib`: https://docs.python.org/3/library/urllib.html
- Documentación de la librería `requests`: https://requests.readthedocs.io/en/master/