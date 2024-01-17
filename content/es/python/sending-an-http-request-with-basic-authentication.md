---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Python: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es una forma de enviar información a un servidor web de manera segura. Los programadores hacen esto para proteger la información confidencial que se envía a través de la red y garantizar que solo el usuario autorizado pueda acceder a ella.

## Cómo hacerlo:

El siguiente código en Python muestra cómo enviar una solicitud HTTP con autenticación básica:

```python
import requests

# Definir nombre de usuario y contraseña
username = "mi_nombre_de_usuario"
password = "mi_contraseña"

# URL del servidor web que se desea acceder
url = "https://mi_servidor_web.com"

# Enviar la solicitud HTTP con autenticación básica
r = requests.get(url, auth=(username, password))

# Imprimir el texto de la respuesta
print(r.text)
```

El resultado de este código será el contenido de la página web a la que se accede, siempre y cuando las credenciales proporcionadas sean correctas.

## Profundizando:

- **Contexto histórico:** La autenticación básica es una de las primeras formas de autenticación utilizadas en Internet. Fue introducida en la versión 1.0 del protocolo HTTP en 1996 y sigue siendo ampliamente utilizada en la actualidad.
- **Alternativas:** Aunque la autenticación básica sigue siendo una forma efectiva de proteger información en una solicitud HTTP, existen otras opciones más seguras como la autenticación digest y la autenticación OAuth.
- **Detalles de implementación:** La autenticación básica utiliza el encabezado `Authorization` en la solicitud HTTP para enviar el nombre de usuario y la contraseña codificados en formato Base64. Sin embargo, es importante destacar que este método no encripta los datos, lo cual puede ser un riesgo si se envía información sensible.

## Ver también:

- [Documentación oficial de Python para el módulo `requests`](https://docs.python-requests.org/en/latest/)
- [Más información sobre autenticación básica en HTTP](https://www.freeformatter.com/http-authentication.html)