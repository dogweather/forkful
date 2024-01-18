---
title:                "Envío de una solicitud http con autenticación básica"
html_title:           "Lua: Envío de una solicitud http con autenticación básica"
simple_title:         "Envío de una solicitud http con autenticación básica"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP con autenticación básica es una forma de comunicarse con un servidor web a través de un protocolo seguro. Los programadores lo hacen para acceder y manipular información de forma segura.

## ¿Cómo hacerlo?

Aquí hay un ejemplo de cómo enviar una solicitud HTTP con autenticación básica utilizando Lua:

```Lua
-- Importar la librería http para usarla en nuestras solicitudes
local http = require("socket.http")

-- Crear una tabla con las credenciales necesarias
local credentials = {
  username = "usuario",
  password = "contraseña"
}

-- Convertir la tabla en una cadena de texto codificada en base64
local encoded_credentials = mime.b64(credentials.username .. ":" .. credentials.password)

-- Construir la URL de la solicitud con la autenticación básica incluida
local url = "https://example.com/api/data"
local headers = { Authorization = "Basic " .. encoded_credentials }

-- Realizar la solicitud utilizando la librería http
local response, status = http.request(url, headers)

-- Imprimir la respuesta recibida del servidor
print(response)
```

Este código enviará una solicitud HTTP GET a "https://example.com/api/data" con las credenciales incluidas en la cabecera de autenticación básica. La respuesta del servidor se almacenará en la variable "response" y se imprimirá en la consola.

## Profundizando

La autenticación básica es una forma de autenticación HTTP que se ha utilizado desde los primeros días de la web. Aunque es una forma sencilla de autenticación, no es la más segura y puede ser vulnerada por ataques de fuerza bruta. Una alternativa más segura es la autenticación HTTP digest, que utiliza un algoritmo de hash en lugar de enviar las credenciales en texto plano.

Para implementar la autenticación básica en Lua, se puede utilizar la librería socket.http, que permite realizar solicitudes HTTP de forma sencilla. También se pueden utilizar otras librerías externas como lua-openssl para manejar la codificación en base64.

## Vea también

Para obtener más información sobre cómo enviar solicitudes HTTP con Lua, puede consultar la documentación oficial de la librería socket.http. También puede explorar otras opciones de autenticación y seguridad disponibles en Lua, como la librería luaossl para cifrado y el framework OpenResty para aplicaciones web seguras.