---
title:                "Python: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP con autenticación básica puede ser necesario cuando se accede a una API o un sitio web que requiere una verificación de identidad para un uso seguro y protegido.

## Cómo hacerlo

```Python
import requests

url = "https://ejemplo.com/api/login"

# Establecer las credenciales de autenticación básica
usuario = "usuario123"
contraseña = "contraseña123"

# Crear un objeto de autenticación básica
autenticacion = requests.auth.HTTPBasicAuth(usuario, contraseña)

# Hacer una solicitud GET a la URL con la autenticación básica incluida
respuesta = requests.get(url, auth=autenticacion)

# Imprimir el código de estado de la respuesta
print(respuesta.status_code)

# Imprimir el contenido de la respuesta en formato JSON
print(respuesta.json())
```

**Salida:**

```
200
{'mensaje': '¡Sesión iniciada correctamente!'}
```

## Profundizando

La autenticación básica se basa en enviar una solicitud con un encabezado de autorización que contiene el nombre de usuario y la contraseña codificados en base64. Este método es simple y ampliamente utilizado, pero no es tan seguro como otros métodos de autenticación como OAuth.

Una solicitud con autenticación básica sigue el siguiente formato:

```
GET /ruta/del/recurso HTTP/1.1
Host: example.com
Authorization: Basic dXNlcjE6Y29udHJhc2UxMjM=
```

La cadena de texto después de "Basic" es el nombre de usuario y la contraseña en formato base64. Esto significa que es importante proteger la URL de la solicitud para evitar que las credenciales de autenticación sean expuestas.

## Ver también

- [Documentación de HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [Tutorial de Requests en Python](https://requests.readthedocs.io/es/latest/user/authentication.html)
- [Ejemplo de autenticación básica en GitHub](https://docs.github.com/es/free-pro-team@latest/rest/overview/other-authentication-methods#basic-authentication)