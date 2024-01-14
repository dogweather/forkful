---
title:                "Python: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué enviar una solicitud HTTP es importante

Enviar una solicitud HTTP es una parte esencial de la programación en Python. Permite a los desarrolladores interactuar con servidores web y obtener información valiosa de manera sencilla y eficiente. Sin esta habilidad, las aplicaciones web no podrían comunicarse con otros sitios y plataformas.

## Cómo enviar una solicitud HTTP en Python

Para enviar una solicitud HTTP en Python, puedes utilizar el módulo `requests`. Primero, importa el módulo en tu código:

```python
import requests
```

Luego, tienes que especificar la URL a la que deseas enviar la solicitud. En este ejemplo, utilizaremos la API de GitHub para obtener información de usuario:

```python
url = "https://api.github.com/users/juansmith"
```

Ahora, puedes enviar una solicitud utilizando el método `get()` del módulo `requests`:

```python
response = requests.get(url)
```

Finalmente, puedes obtener el resultado de la solicitud a través del atributo `text`. En este caso, mostraremos el nombre de usuario y la biografía del usuario en la consola:

```python
print(response.json()["login"])
# output: juansmith

print(response.json()["bio"])
# output: Full-stack developer and Python enthusiast.
```

¡Y eso es todo! Ahora, ya sabes cómo enviar una solicitud HTTP en Python.

## Profundizando en las solicitudes HTTP

Una solicitud HTTP es simplemente una petición de información a un servidor web. Puedes personalizar tu solicitud para obtener diferentes tipos de datos, como JSON, HTML, imágenes, etc. También puedes agregar parámetros y encabezados a tu solicitud para filtrar los resultados.

Además, es importante considerar los códigos de estado de respuesta que devuelve el servidor web. Por ejemplo, el código 200 significa que la solicitud se completó con éxito, mientras que el código 404 indica que no se encontró la información solicitada.

Puedes explorar más sobre las solicitudes HTTP y su uso en la documentación de `requests` y en otros recursos en línea.

## Ver también

- [Documentación de `requests`](https://requests.readthedocs.io/en/master/)
- [Tutorial de HTTP para principiantes](https://www.tutorialspoint.com/http/index.htm)
- [Guide to Python's Requests Library](https://www.dataquest.io/blog/python-API-tutorial/) (en inglés)