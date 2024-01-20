---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Enviar um pedido HTTP com autenticação básica é o processo de fazer um pedido a um servidor web usando um nome de usuário e senha. Programadores fazem isso para acessar recursos protegidos em servidores web.

## Como Fazer:
Vamos realizar este processo usando a biblioteca Lua 'http' e 'ltn12'. O código a seguir demonstra como enviar uma solicitação HTTP básica com autenticação:

```Lua
-- Importa as bibliotecas necessárias
local http = require('socket.http')
local ltn12 = require('ltn12')

-- Define o URL, nome de usuário e senha
local url = 'http://seu-servidor.com'
local auth = 'Basic ' .. (mime.b64('usuario:senha'))

-- Protocolo HTTP GET com autenticação básica
local response_body, http_status, http_headers, http_status_line = http.request {
    url = url,
    method = 'GET',
    headers = {
        Authorization = auth
    },
    sink = ltn12.sink.table(response_body_table)
}

-- Imprime os detalhes da resposta
print(http_status)
print(http_headers)
print(http_status_line)
print(table.concat(response_body_table, ''))
```
Quando você executa este script, receberá a resposta do servidor, o status HTTP, os cabeçalhos HTTP e linha de status de HTTP.

## Aprofundando
Enviar uma solicitação HTTP com autenticação básica é um procedimento comum, mas nem sempre tem sido assim. Nos primeiros dias da web, a autenticação não era comum e nem mesmo uma prática padrão. No entanto, com o aumento da necessidade de segurança, a autenticação básica tornou-se uma prática comum.

Existem várias alternativas para a autenticação básica, incluindo a autenticação Digest e a autenticação baseada em token. Cada uma tem suas próprias vantagens e desvantagens.

Sobre a implementação em Lua, usamos a biblioteca socket.http que fornece funções para fazer solicitações HTTP e a biblioteca ltn12 para tratar o corpo da resposta. Codificar a string de autenticação em Base64 é feito pela biblioteca mime.

## Veja Também
Para mais informações, consulte os seguintes recursos:
- Documentação do LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- Documentação do Lua MIME: https://luarocks.org/modules/luarocks/mime
- RFC 7617 (Autenticação Básica HTTP): https://tools.ietf.org/html/rfc7617
Por favor, note que a segurança na web é um campo grande e em constante evolução, por isso é importante se manter atualizado com as melhores práticas e desenvolvimentos recentes.