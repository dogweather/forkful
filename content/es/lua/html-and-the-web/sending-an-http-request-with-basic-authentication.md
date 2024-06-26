---
date: 2024-01-20 18:02:03.241373-07:00
description: "C\xF3mo Hacerlo: Usaremos el m\xF3dulo `socket.http` para realizar la\
  \ solicitud y `mime` para codificar las credenciales."
lastmod: '2024-03-13T22:44:59.201480-06:00'
model: gpt-4-1106-preview
summary: "Usaremos el m\xF3dulo `socket.http` para realizar la solicitud y `mime`\
  \ para codificar las credenciales."
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
weight: 45
---

## Cómo Hacerlo:
Usaremos el módulo `socket.http` para realizar la solicitud y `mime` para codificar las credenciales.

```Lua
-- Importamos los módulos necesarios
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Credenciales 
local username = "tu_usuario"
local password = "tu_contraseña"

-- Codificamos las credenciales en Base64
local auth = "Basic " .. mime.b64(username .. ":" .. password)

-- Preparamos la cabecera de autenticación
local headers = {
    ["Authorization"] = auth
}

-- Configuramos los parámetros para la solicitud
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://tuurl.com/api/datos",
    method = "GET",
    headers = headers,
    sink = ltn12.sink.table(response_body)
}

-- Verifica si la solicitud fue exitosa
if code == 200 then
    print(table.concat(response_body))
else
    print("Error: " .. (res or code))
end
```

El código anterior realiza una solicitud GET a una API, incluyendo las credenciales codificadas en Base64 en la cabecera de autorización.

## Profundizando
La autenticación básica (Basic Auth) se ha utilizado desde los primeros días de la web. No es la más segura porque las credenciales son relativamente fáciles de descifrar. Hoy existen métodos más protegidos como OAuth. En Lua, la implementación es directa: codifica las credenciales, añade la cabecera, envía la solicitud. Lua no incluye nativamente soporte para HTTP o codificación Base64, por eso usamos los módulos externos `socket.http` y `mime`, respectivamente.

## Ver También
- [Documentación de LuaSec](https://github.com/brunoos/luasec/wiki) - Para realizar conexiones seguras HTTPS.
- [Manual de Lua 5.4 (versión actual)](http://www.lua.org/manual/5.4/) - Referencia del lenguaje.
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617) - Descripción del método de autenticación básica.
- [LuaRocks](https://luarocks.org/) - Gestor de paquetes de Lua donde puedes encontrar `socket.http` y `mime`.
