---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##¿Qué & Por qué?

En programación, enviar una solicitud HTTP con autenticación básica implica una comunicación entre el cliente y el servidor utilizando el protocolo HTTP. Los programadores lo hacen para restringir el acceso no autorizado y permitir solo a los usuarios con credenciales válidas interactuar con el servidor.

## ¿Cómo hacerlo?

Para enviar una solicitud HTTP con autenticación básica en Lua, puedes usar el paquete de socket Lua y su extensión http. Aquí hay un ejemplo del código:

```Lua
local http = require('socket.http')
local ltn12 = require('ltn12')

local url = 'http://example.com'
local user = 'username'
local password = 'password'

local auth = 'Basic ' .. (user .. ':' .. password):gmatch('(.-)$'):)

http.request{
  url = url,
  headers = { authorization = auth },
  sink = ltn12.sink.file(io.stdout),
}
```

Este código accederá a 'http://example.com' con el nombre de usuario y la contraseña proporcionados.

## Profundizando

Enviar una solicitud HTTP con autenticación básica es un método antiguo y bien establecido para garantizar la seguridad. Sin embargo, debido a que las credenciales no están encriptadas, es vulnerable a los ataques de 'man-in-the-middle'. Para mejorar la seguridad, se introdujo la autenticación Digest.

Alternativamente, puedes considerar el uso de bibliotecas como LuaSec para implementar la autenticación HTTPS, que proporciona una capa de seguridad adicional.

La implementación detallada de las solicitudes HTTP con autenticación básica en Lua puede variar dependiendo de la biblioteca que elijas para realizar las solicitudes HTTP. La mayoría de las bibliotecas proporcionan funciones similares a las descritas anteriormente.

## Ver también

Para obtener más información sobre la programación en Lua y la autenticación básica, puedes consultar estas fuentes:

- [HTTP Authentication](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.8)
- [Programming in Lua](https://www.lua.org/pil/)
- [LuaSocket library](https://w3.impa.br/~diego/software/luasocket/)