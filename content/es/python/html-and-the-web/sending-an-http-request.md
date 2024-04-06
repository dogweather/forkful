---
date: 2024-01-20 18:00:13.264394-07:00
description: "How to: (C\xF3mo hacerlo:) Output."
lastmod: '2024-04-05T21:53:59.966266-06:00'
model: gpt-4-1106-preview
summary: "(C\xF3mo hacerlo:) Output."
title: Enviando una solicitud http
weight: 44
---

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
