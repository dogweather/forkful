---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

En algún momento, como programadores, necesitamos enviar una solicitud HTTP desde nuestros programas para interactuar con aplicaciones basadas en la web. Esto se hace para obtener datos de la web, publicar datos, interactuar con APIs RESTful y muchos otros usos.

## Cómo hacerlo:

Lua, por defecto, no ofrece una forma de enviar una solicitud HTTP, por lo que necesitaremos usar una librería llamada luarocks:

```Lua
-- Instalar luarocks
os.execute('sudo luarocks install luasocket')
os.execute('sudo luarocks install luasec')
```

Cuando hayamos instalado luarocks, podemos usar el paquete `socket.http` para realizar una solicitud HTTP. Aquí hay un ejemplo:

```Lua
socket = require('socket.http')

-- URL para enviar la solicitud HTTP
url = "http://httpbin.org/ip"

-- Enviar la solicitud HTTP
response_body, status_code, headers, status_text = socket.request(url)

-- Imprimir el cuerpo de la respuesta
if status_code == 200 then
    print(response_body)
else
    print("HTTP request failed with status: " .. status_code)
end
```

Cuando ejecutes este código, deberías ver una salida como esta:

```Lua
{
  "origin": "tu.dirección.IP.aquí"
}
```

## Perspectiva más Profunda:

Las solicitudes HTTP se utilizan para la comunicación cliente-servidor. Nacieron con la web y han evolucionado desde la versión HTTP/0.9 en 1991 hasta la actualidad con HTTP/2. Realizar una solicitud HTTP en Lua no siempre fue tan sencillo. Las primeras versiones de Lua no tenían este tipo de soporte y la solicitud HTTP necesitaba implementarse en C.

Hoy, Lua proporciona una forma sencilla de realizar solicitudes HTTP a través de la extensibilidad de luarocks y la biblioteca luasocket. Sin embargo, hay alternativas a luasocket como lua-http, que es una librería de protocolo HTTP y WebSocket que tiene más funciones.

Con respecto a la implementación, `socket.http.request` de luasocket crea un socket, lo conecta a la URL dada, envía la solicitud HTTP, recibe la respuesta y la devuelve al código del llamante.

## Ver También:

Para una profundización en el uso de solicitudes HTTP en Lua, consulta estos enlaces:

- Documentación oficial de Lua Socket: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaHTTP - Una biblioteca de protocolo HTTP y WebSocket: https://github.com/daurnimator/lua-http
- Tutorial completo en inglés: https://www.tutorialspoint.com/lua/lua_http_requests.htm