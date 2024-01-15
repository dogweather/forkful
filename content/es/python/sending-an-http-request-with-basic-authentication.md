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

## Por qué

Enviar una solicitud de HTTP con autenticación básica es una forma segura de acceder a recursos en línea protegidos por una contraseña. Esta autenticación se utiliza comúnmente en aplicaciones y servicios web para verificar la identidad del usuario y garantizar la privacidad y seguridad de la información.

## Cómo hacerlo

Para enviar una solicitud de HTTP con autenticación básica en Python, necesitarás importar el módulo "requests" y usar el método "get" o "post", dependiendo de la acción que desees realizar. A continuación, proporciona la URL del recurso que deseas acceder y utiliza el parámetro "auth" para especificar las credenciales de autenticación.

```
import requests

url = "https://ejemplo.com/recursos"

resp = requests.get(url, auth=('usuario', 'contraseña'))

print(resp)
```

La respuesta de la solicitud mostrará un código de estado 200 si fue exitosa, o un código de error si hubo algún problema con la autenticación.

## Profundizando

Básicamente, la autenticación básica funciona enviando un encabezado "Authorization" con cada solicitud que contiene el nombre de usuario y la contraseña en formato "usuario:contraseña" codificado en base64. Esto se puede hacer manualmente usando la función "b64encode" del módulo "base64", pero en Python, se puede hacer más fácilmente utilizando el parámetro "auth" como se mostró anteriormente.

Además, hay otras formas de autenticación en HTTP, como OAuth y JSON Web Token (JWT), que proporcionan un nivel extra de seguridad. Sin embargo, la autenticación básica sigue siendo una opción simple y efectiva para muchos casos de uso.

## Ver también

- [Documentación de requests en Python](https://docs.python-requests.org/en/master/)
- [Tutorial de autenticación básica en requests](https://requests.readthedocs.io/en/master/user/authentication/#basic-authentication)
- [Ejemplo de autenticación básica en una API en línea](https://docs.ruby-lang.org/en/master/Net/HTTP.html#class-Net::HTTP-label-Basic+Authentication) (en Ruby)