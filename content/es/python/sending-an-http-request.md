---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

(What & Why?)

Enviar una solicitud HTTP significa pedir datos a un servidor web. Los programadores lo hacen para interactuar con servicios web, obteniendo información o enviando datos para su procesamiento.

## Como hacerlo:

(How to:)

Para enviar solicitudes HTTP en Python, podemos utilizar el paquete `requests`. Echemos un vistazo a un ejemplo simple de cómo hacerlo.

```Python
import requests

respuesta = requests.get('http://example.com')
print(respuesta.text)
```
Si ejecutas este código, obtendrás el contenido HTML de `example.com` impreso en tu pantalla.

## Profundización:

(Deep Dive)

El protocolo HTTP es un pilar de la web tal como la conocemos y se remonta a 1991. Python introdujo la biblioteca `requests` como una forma simple de trabajar con HTTP, y ha permanecido como la forma predominante de hacerlo en Python desde entonces.

Existen alternativas a la biblioteca `requests`, algunas de las cuales son `http.client` (incluido en la biblioteca estándar de Python) y `httplib2`.

Cuando se envía una solicitud HTTP con `requests`, primero se establece una conexión con el servidor. Luego, se envía una solicitud que incluye una línea de solicitud (método, URL, versión de HTTP), encabezados de solicitud, y un cuerpo de solicitud (datos a enviar, si los hay). El servidor responde con su propio mensaje, que también incluye una línea de estado, encabezados de respuesta y un cuerpo de respuesta (los datos que está enviando de vuelta).

## Ver También:

(See Also:)

Aprende más sobre solicitudes HTTP y sobre la biblioteca `requests`:

1. Documentación oficial de `requests`: https://docs.python-requests.org/en/latest/
2. Introducción a HTTP en Mozilla Developer Network: https://developer.mozilla.org/es/docs/Web/HTTP/Overview
3. Tutorial de `http.client`: https://docs.python.org/3/library/http.client.html
4. Documentación de `httplib2`: https://httplib2.readthedocs.io/en/latest/