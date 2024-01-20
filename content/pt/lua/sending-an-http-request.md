---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Enviar uma solicitação HTTP em Lua é o método pelo qual seu programa pode se comunicar com outros programas via web. Isso é útil para obter dados de APIs web, enviar informações para servidores e interagir com outros serviços na internet.

## Como fazer:

Para realizar uma solicitação HTTP em Lua, você precisará do módulo `socket.http`. Aqui está um exemplo simples:

```Lua
local http = require('socket.http')
local url = "http://httpbin.org/get"

-- Enviando uma solicitação GET
local response, status = http.request(url)
if status == 200 then
    print(response)
else
    print("Erro: "..status)
end
```

Neste exemplo, o código envia uma solicitação GET para 'http://httpbin.org/get'. Se a solicitação for bem-sucedida (status HTTP 200), imprime a resposta.

## Mergulho profundo:

O módulo `socket.http` usado no exemplo acima faz parte da biblioteca LuaSocket, que fornece funcionalidades de rede de baixo nível para Lua desde 2004. Ele é um dos muitos pacotes disponíveis que tornam as solicitações HTTP possíveis em Lua, embora seja um dos mais populares e amplamente usados.

Há algumas alternativas ao LuaSocket. Por exemplo, o módulo 'http' do framework Turbo.lua é uma excelente escolha se você estiver trabalhando nesse framework. Você também pode optar por bibliotecas como lua-http ou lua-resty-http se estiver procurando algo mais atual ou específico.

Quando você envia uma solicitação HTTP usando LuaSocket, o que acontece em segundo plano é que LuaSocket abre um soquete TCP, envia uma solicitação HTTP formatada corretamente pelo soquete e, em seguida, lê a resposta antes de fechá-lo.

## Veja também:

- Documentação LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- Documentação Turbo.lua: https://turbo.readthedocs.io/en/latest/
- lua-http no GitHub: https://github.com/daurnimator/lua-http
- lua-resty-http no GitHub: https://github.com/ledgetech/lua-resty-http