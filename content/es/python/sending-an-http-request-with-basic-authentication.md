---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
El envío de una solicitud HTTP con autenticación básica se refiere a la transmisión segura de sus credenciales (nombre de usuario y contraseña) a un servidor en la red. Los programadores suelen hacerlo para establecer una conexión segura y proteger la privacidad del usuario mientras interactúa con aplicaciones en la web.

## Cómo hacerlo:
Para enviar una solicitud HTTP con autenticación básica en Python, usaremos la biblioteca `requests`. Aquí hay un ejemplo:

```Python
import requests
from requests.auth import HTTPBasicAuth

respuesta = requests.get('https://miweb.com', auth=HTTPBasicAuth('usuario', 'contraseña'))

print(respuesta.status_code)
```
El código anterior realiza una solicitud GET a 'miweb.com' utilizando autenticación básica. 'usuario' y 'contraseña' son tus credenciales. Este script imprime el código de estado de la respuesta HTTP.

## Buceo profundo:
El protocolo HTTP y la autenticación básica han sido estándares de la industria desde los primeros días de la web. Aunque efectiva y fácil de implementar, la autenticación básica transfiere las contraseñas en texto sin encriptar, lo que puede ser un riesgo para la seguridad en entornos no seguros.

Alternativas a la autenticación básica pueden incluir OAuth, Token JWT (JSON Web Tokens) o autenticación de digest, todos ofreciendo sus propias ventajas en términos de seguridad y flexibilidad.

Cuando utilizas la autenticación básica en Python, la biblioteca `requests` codifica automáticamente tus credenciales en base64 antes de enviarlas en el encabezado HTTP. Sin embargo, considera siempre usar conexiones HTTPS en lugar de HTTP para asegurar que tus credenciales sean encriptadas durante la transmisión.

## Ver también:
1. Documentation oficial de Python para la biblioteca `requests`: https://docs.python-requests.org/en/master/
2. Autenticación básica en la documentación de Mozilla: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#autenticación_básica
3. Tutorial de autenticación con Python y HTTP: https://realpython.com/python-basic-authentication/