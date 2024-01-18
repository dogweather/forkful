---
title:                "Enviando una solicitud http"
html_title:           "Lua: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es una acción común entre los programadores. Se trata de enviar una solicitud a un servidor web para obtener cierta información o realizar una acción determinada. Los programadores normalmente realizan esto para interactuar con aplicaciones web o servicios en línea.

## Cómo hacerlo:

Aquí tienes un ejemplo en Lua de cómo enviar una solicitud HTTP utilizando la biblioteca de la red [socket](http://w3.impa.br/~diego/software/luasocket/):

```lua
local socket = require("socket") 

-- establacer conexión con el servidor 
local connection = socket.tcp() 
connection:connect("www.example.com", 80)  

-- enviar la solicitud y recibir la respuesta
connection:send("GET / HTTP/1.0\r\n\r\n") 
local response = connection:receive("*a") 
print(response)

-- cerrar la conexión
connection:close()
```

La salida del código anterior debería ser algo similar a esto:

```
HTTP/1.0 200 OK
Date: Tue, 10 Aug 2021 18:00:00 GMT
Server: Apache
Content-Type: text/html; charset=UTF-8
Content-Length: 76
Connection: close

<html>
<head><title>Bienvenido</title></head>
<body>¡Hola mundo!</body>
</html>
```

## Profundizando:

Enviar solicitudes HTTP se ha vuelto aún más importante con el auge de las aplicaciones web y servicios en línea. Antes, esta acción estaba limitada a navegadores web, pero ahora los programadores pueden realizar solicitudes utilizando diferentes lenguajes de programación, incluyendo Lua. Existen otras bibliotecas en Lua que también permiten enviar solicitudes HTTP, como [LuaHTTP](https://github.com/daurnimator/lua-http) y [Lua-cURL](https://github.com/Lua-cURL/Lua-cURLv3).

## Ver también:

- La documentación oficial de Lua sobre la biblioteca [socket](https://www.lua.org/manual/5.4/manual.html#9.1)
- Un tutorial sobre cómo enviar solicitudes HTTP utilizando [LuaHTTP](https://www.programering.com/a/MDN5MzMwATc.html)
- Una guía paso a paso para enviar solicitudes HTTP utilizando [Lua-cURL](https://riptutorial.com/curl/example/23155/enviar-una-solicitud-http-con-curl)