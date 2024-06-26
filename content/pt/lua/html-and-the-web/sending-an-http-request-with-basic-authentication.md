---
date: 2024-01-20 18:02:05.405162-07:00
description: "Como Fazer: Para enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\
  \xE3o b\xE1sica em Lua, vamos usar o m\xF3dulo `socket.http` e `mime`. Primeiro,\
  \ instale o m\xF3dulo\u2026"
lastmod: '2024-03-13T22:44:46.708769-06:00'
model: gpt-4-1106-preview
summary: "Para enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica\
  \ em Lua, vamos usar o m\xF3dulo `socket.http` e `mime`."
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## Como Fazer:
Para enviar uma solicitação HTTP com autenticação básica em Lua, vamos usar o módulo `socket.http` e `mime`. Primeiro, instale o módulo `luasocket` se ainda não estiver instalado:

```sh
luarocks install luasocket
```

A seguir, um exemplo de como fazer uma requisição GET com autenticação:

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Credenciais
local username = "usuario"
local password = "senha"

-- Codifica as credenciais
local auth = "Basic " .. mime.b64(username .. ":" .. password)

local response_body = {}

-- Realiza a requisição HTTP GET
http.request{
    url = "http://seuservico.com/recurso",
    method = "GET",
    headers = {
        ["Authorization"] = auth
    },
    sink = ltn12.sink.table(response_body)
}

-- Concatena o corpo da resposta e imprime
print(table.concat(response_body))
```

Saída de exemplo:
    
```plaintext
{ "dados": "valor", "maisDados": "outroValor" }
```

## Aprofundamento
A autenticação básica HTTP é um método antigo e simples de proteger recursos: não é o mais seguro, mas é fácil de implementar. Uma alternativa moderna e mais segura é a autenticação via tokens, como OAuth. Ao fazer uma solicitação com autenticação básica, o usuário e a senha são codificados em Base64, mas não criptografados, o que pode ser inseguro em conexões não HTTPS.

Outro detalhe é que, ao usar Lua para requisições HTTP, o luasocket é um dos módulos mais populares, mas existem alternativas como o `LuaSec` para a conexão segura HTTPS. Não esqueça de gerenciar adequadamente a privacidade das credenciais para evitar vazamentos de informações sensíveis.

## Ver Também
- [Documentação do LuaSocket](http://w3.impa.br/~diego/software/luasocket/http.html)
- [Documentação do MIME (luasocket)](http://w3.impa.br/~diego/software/luasocket/mime.html)
- [Tutorial de LuaSec para HTTPS](https://github.com/brunoos/luasec/wiki)
- [Autenticação Básica no MDN](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#autentica%C3%A7%C3%A3o_b%C3%A1sica)
