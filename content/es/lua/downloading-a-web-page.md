---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Descargar una página web significa traer todo su contenido a tu dispositivo. Los programadores lo hacen para analizar, extraer datos o para hacer pruebas sin conexión.

## Cómo:

```Lua
-- Utiliza la biblioteca LuaSocket
local http = require("socket.http")

-- URL para descargar
local url = "http://www.example.com"

-- Descarga la página
local body, code, headers, status = http.request(url)

-- Verifica si la descarga fué exitosa
if code == 200 then
  print(body)
else
  print("Error: " .. status)
end
```
En este código, usamos la biblioteca LuaSocket para hacer una solicitud HTTP. Si todo sale bien, imprimiremos el contenido de la página. De lo contrario, mostraremos un mensaje de error.

## Profundización

Lua no tenía una función incorporada para descargar páginas web en sus primeras versiones. Sin embargo, muchas bibliotecas como LuaSocket han surgido para llenar este vacío. No obstante, hay otros enfoques como IO-Lua, aunque requerirán un poco de trabajo extra para manejar las conexiones HTTP.

En cuanto a los detalles de implementación, LuaSocket abre una conexión TCP con el servidor web y envía una solicitud GET HTTP. Luego recoge la respuesta, que es el contenido de tu página web.

## Ver También

- [HTTP Programming in Lua](http://w3.impa.br/~diego/software/luasocket/http.html)
- [LuaSocket Reference Manual](http://w3.impa.br/~diego/software/luasocket/reference.html)
- [Scraping in lua](https://lua-users.org/wiki/ScrapingHtml)