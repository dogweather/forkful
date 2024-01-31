---
title:                "Enviando una solicitud http"
date:                  2024-01-20T18:00:13.264394-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Qué y Por Qué?)
Enviar una solicitud HTTP significa pedirle algo a un servidor web. Programadores hacen esto para interactuar con APIs, descargar archivos o comunicarse con otros servicios web.

## How to: (Cómo hacerlo:)
```Python
import requests

# Realizar una solicitud GET
respuesta = requests.get('https://jsonplaceholder.typicode.com/posts/1')

# Muestra el resultado
if respuesta.status_code == 200:
    print(respuesta.json())  # En caso de éxito, imprime la respuesta JSON
else:
    print(f'Error: {respuesta.status_code}')  # Imprime un error si algo salió mal
```
Output:
```
{
  'userId': 1,
  'id': 1,
  'title': 'sunt aut facere repellat provident occaecati excepturi optio reprehenderit',
  'body': 'quia et suscipit\nsuscipit recusandae consequuntur expedita et cum... [truncated output]'
}
```
## Deep Dive (Buceo Profundo)
El concepto de enviar solicitudes HTTP comenzó poco después de la creación de la web. Alternativas a `requests` incluyen las bibliotecas `http.client` en la biblioteca estándar de Python o `aiohttp` para asincronía. `Requests` maneja muchos detalles del proceso de la solicitud, como codificación de parámetros, manejo de cookies, y más, facilitando la vida a los desarrolladores.

## See Also (Ver También)
- Documentación de `requests`: https://requests.readthedocs.io
- API JSONPlaceholder para pruebas: https://jsonplaceholder.typicode.com/
- Tutorial oficial de Python sobre HTTP: https://docs.python.org/3/library/http.html
