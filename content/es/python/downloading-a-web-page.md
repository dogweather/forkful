---
title:                "Descargando una página web."
html_title:           "Python: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web es algo que puede ser útil para muchas cosas, como por ejemplo, obtener información de una página que puede ser útil para tu trabajo o simplemente para tener una copia de esa página por si acaso deja de estar disponible en el futuro.

## Cómo hacerlo

Descargar una página web es algo que puede ser muy sencillo con Python gracias al módulo `urllib.request`. Primero, importa el módulo en tu código:

```python
import urllib.request
```

A continuación, utiliza la función `urlretrieve()` para descargar la página web especificando la URL de la página y el nombre de archivo en el que se guardará:

```python
urllib.request.urlretrieve("https://ejemplo.com", "archivo.html")
```

Al ejecutar este código, la página web se descargará y se guardará en un archivo llamado "archivo.html". Puedes verificar que la descarga se realizó correctamente abriendo el archivo en un navegador web.

## Profundizando

El módulo `urllib.request` también ofrece una función llamada `urlopen()` que puede ser útil si quieres obtener más información de la página web que estás descargando. Esta función te permite obtener un objeto de tipo `HTTPResponse` que contiene información como el código de respuesta, encabezados y contenido de la página.

Por ejemplo, podemos utilizar la función `urlopen()` para obtener información del encabezado de la página web:

```python
respuesta = urllib.request.urlopen("https://ejemplo.com")
print(respuesta.headers)
```

Esto imprimirá un diccionario con todos los encabezados de la página, como "content-type" y "content-length". Además, también podemos utilizar la función `read()` en el objeto `HTTPResponse` para obtener el contenido de la página como bytes y luego decodificarlo a una cadena:

```python
contenido = respuesta.read().decode("utf-8")
```

De esta manera, podemos tener acceso tanto a la información del encabezado como al contenido de la página web descargada.

## Ver también

- Documentación del módulo `urllib.request`: https://docs.python.org/es/3/library/urllib.request.html
- Tutorial de Python para principiantes: https://www.python.org/about/gettingstarted/