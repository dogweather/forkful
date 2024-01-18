---
title:                "Enviando uma solicitação http"
html_title:           "Lua: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# O Que & Por Que?

Enviar uma solicitação HTTP significa realizar um pedido para acessar um recurso em um servidor remoto. Programadores fazem isso para obter informações ou enviar dados entre diferentes sistemas.

# Como Fazer:

```
-- Exemplo de código para enviar uma solicitação HTTP usando a biblioteca LuaSocket

local http = require("socket.http") -- Importa a biblioteca LuaSocket

-- Faz uma solicitação GET para o site da NASA para obter as notícias mais recentes
local response_body, response_code, response_headers = http.request("https://www.nasa.gov/rss/dyn/breaking_news.rss")

-- Imprime o código de resposta e o conteúdo da resposta
print(response_code)
print(response_body)

-- Saída esperada: 200 (Código de resposta OK) e o RSS feed com as notícias mais recentes da NASA
```

```
-- Exemplo de código para enviar uma solicitação HTTP utilizando a biblioteca Lua HTTPClient

local http_client = require("http_client") -- Importa a biblioteca Lua HTTPClient

-- Define as opções para a solicitação
local options = {
    url = "https://jsonplaceholder.typicode.com/posts", -- URL do recurso a ser acessado
    method = "POST", -- Método da solicitação (GET, POST, PUT, DELETE, etc.)
    headers = { ["Content-Type"] = "application/json"}, -- Cabeçalhos da requisição
    body = "{ \"title\": \"Minha primeira postagem\", \"body\": \"Olá Mundo\", \"userId\": 1 }" -- Dados a serem enviados
}

-- Realiza a solicitação
local response = http_client(options)

-- Imprime o corpo da resposta
print(response.body)

-- Saída esperada: "{ \"title\": \"Minha primeira postagem\", \"body\": \"Olá Mundo\", \"userId\": 1 }" (Dados enviados no corpo da solicitação)
```

# Mergulho Profundo:

Enviar solicitações HTTP é uma técnica amplamente utilizada na programação para acessar recursos remotos. Existem diversas bibliotecas disponíveis para realizar essa tarefa no Lua, como a LuaSocket e a Lua HTTPClient. Além disso, muitas frameworks e ferramentas também possuem recursos para realizar solicitações HTTP, facilitando ainda mais o processo.

# Veja Também:

- [Documentação da Biblioteca LuaSocket] (https://w3.impa.br/~diego/software/luasocket/http.html)
- [Documentação da Biblioteca Lua HTTPClient] (https://github.com/Mashape/lua-httpclient)