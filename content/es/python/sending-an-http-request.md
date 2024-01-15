---
title:                "Enviando una solicitud http"
html_title:           "Python: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, el envío de solicitudes HTTP es esencial para comunicarse con servidores y obtener datos de la web. Aprender a enviar este tipo de solicitudes te permitirá desarrollar aplicaciones y scripts más potentes y versátiles.

## Cómo hacerlo
```Python
import requests

# Ejemplo de solicitud GET
response = requests.get("https://algunaweb.com/api/usuarios")

# Obtener datos de la respuesta
data = response.json()

# Ejemplo de solicitud POST con datos
payload = {"nombre": "Juan", "apellido": "Pérez"}
response = requests.post("https://algunaweb.com/api/usuarios", data=payload)
```
La librería requests de Python permite realizar solicitudes HTTP de manera sencilla. En el primer ejemplo, estamos realizando una solicitud GET a una API de usuarios y guardando la respuesta en la variable "data". En el segundo ejemplo, estamos haciendo una solicitud POST con datos en formato JSON y guardando la respuesta en la variable "response". Además, es importante tener en cuenta que también se pueden enviar solicitudes PUT, PATCH, DELETE, entre otras, dependiendo de las necesidades de tu aplicación.

## Profundizando
Detrás de cada solicitud HTTP hay un proceso complejo que implica el uso de protocolos de red y encabezados que facilitan la comunicación entre el cliente y el servidor. Al enviar una solicitud HTTP, se establece una conexión TCP con el servidor y se envía una solicitud con una serie de encabezados para especificar el tipo de solicitud y los datos que se están enviando. El servidor, a su vez, responde con una serie de encabezados y el cuerpo de la respuesta que contiene los datos solicitados.

Existen diferentes tipos de solicitudes HTTP, como GET, POST, PUT, PATCH, DELETE, entre otras. Cada una de ellas tiene un propósito específico y se utiliza en diferentes situaciones. Además, también es importante mencionar que existen códigos de estado que indican si la solicitud fue exitosa, si hubo algún error o si es necesario realizar alguna acción adicional.

## Ver también
- [Documentación de requests en español](https://requests.readthedocs.io/es/latest/)
- [Códigos de estado HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Status)
- [Protocolo HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Overview)