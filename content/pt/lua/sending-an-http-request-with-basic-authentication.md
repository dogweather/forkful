---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Lua: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?
Enviar uma solicitação HTTP com autenticação básica é uma maneira de enviar informações para um servidor web de forma segura através de uma conexão HTTP. Os programadores fazem isso para autenticar usuários e garantir que eles tenham permissão para acessar determinados recursos no servidor.

## Como fazer:
```Lua
-- Importar a biblioteca "socket" para fazer uma conexão HTTP
local socket = require("socket")
-- Definir o formato de autenticação básica
local basicAuth = "Basic " .. mime.b64(username .. ":" .. password)
-- Definir as informações da solicitação, incluindo o URL, o método (GET, POST, etc.) e o cabeçalho de autenticação básica
local request = {
  url = "https://example.com/api/resource",
  method = "POST",
  headers = {
    ["Authorization"] = basicAuth
  }
}
-- Enviar a solicitação e receber a resposta do servidor
local response = socket.http.request(request)
-- Imprimir a resposta do servidor
print(response.body)
```

## Deep Dive:
Uma das principais vantagens de usar a autenticação básica é sua simplicidade e ampla compatibilidade com diferentes linguagens de programação e servidores web. No entanto, a segurança da autenticação básica é limitada, pois as informações de autenticação são enviadas em texto simples no cabeçalho da solicitação HTTP. Alternativas mais seguras incluem a autenticação Digest ou o uso de protocolos mais avançados, como o OAuth. A implementação detalhada pode variar dependendo da linguagem de programação e dos frameworks utilizados.

## Veja também:
- [Tutorial de autenticação básica em Lua](https://www.lua.org/cgi-bin/demo?http)
- [Documentação da biblioteca "socket" em Lua](https://w3.impa.br/~diego/software/luasocket/reference.html#request)
- [Comparação de diferentes tipos de autenticação em HTTP](https://www.geeksforgeeks.org/http-authentication/)
- [Mais informações sobre a segurança da autenticação básica em HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#Basic_authentication_scheme)