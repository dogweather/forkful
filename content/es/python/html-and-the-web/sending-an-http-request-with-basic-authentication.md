---
title:                "Enviando una solicitud http con autenticación básica"
aliases:
- /es/python/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:15.998023-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica significa incluir credenciales de usuario y contraseña en una solicitud a un servidor. Los programadores lo hacen para acceder a recursos protegidos que requieren identificación.

## Cómo Hacerlo:

Para enviar una solicitud HTTP con autenticación básica en Python, puedes utilizar la biblioteca `requests`. Aquí tienes un ejemplo rápido:

```python
import requests
from requests.auth import HTTPBasicAuth

url = 'https://ejemplo.com/api/recursos'
usuario = 'tu_usuario'
contraseña = 'tu_contraseña'

respuesta = requests.get(url, auth=HTTPBasicAuth(usuario, contraseña))

print(respuesta.status_code)
print(respuesta.json())
```

Si todo va bien, deberías ver algo así:

```
200
{'mensaje': 'Acceso concedido, datos cargados correctamente.'}
```

## Profundización

La autenticación básica HTTP es un método antiguo y simple, pero no el más seguro, ya que envía credenciales en texto claro codificado en Base64. Fue introducido en la especificación HTTP 1.0 y aún es muy utilizado por su simpleza. Alternativas más seguras incluyen OAuth o JWT (JSON Web Tokens) para una seguridad más robusta.

En Python, la biblioteca `requests` simplifica este proceso enormemente. Sin embargo, para entornos de producción, es recomendable utilizar la autenticación sobre HTTPS para evitar la exposición de credenciales.

La implementación del lado del cliente es simple, pero del lado del servidor, se requiere validar las credenciales contra una base de datos o un servicio de directorio.

## Ver También

- Documentación oficial de la biblioteca `requests`: https://docs.python-requests.org/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Guía sobre alternativas más seguras a la Autenticación Básica HTTP: https://www.owasp.org/index.php/Authentication_Cheat_Sheet
