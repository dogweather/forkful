---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Escrever testes é o processo de criar scripts que automaticamente testam seu código para checar a integridade e corretude das funcionalidades. Programadores fazem isso para garantir que novas mudanças não quebrem funcionalidades existentes e para facilitar a manutenção do código ao longo do tempo.

## Como Fazer:
```Lua
-- Instale o Lua com um gerenciador de pacotes como luarocks
-- luarocks install luaunit

local luaunit = require('luaunit')
local Calculadora = {}

function Calculadora.soma(a, b)
    return a + b
end

-- Definição de teste
function testSoma()
    luaunit.assertEquals(Calculadora.soma(2, 2), 4)
end

-- Rodando os testes
os.exit(luaunit.LuaUnit.run())
```

Saída do exemplo:
```
.
Ran 1 tests in 0.001 seconds, 1 success, 0 failures
```

## Mergulho Profundo
1. **Contexto Histórico**: Testes automatizados têm suas raízes na prática de "debugging" do software, que começou logo após os primeiros programas terem sido escritos.

2. **Alternativas**: Existem outras frameworks de teste no Lua, como o Busted e o TestMore, que oferecem diferentes funcionalidades e sintaxes.

3. **Detalhes da Implementação**: Ao escrever testes, considere práticas como TDD (Test-Driven Development), onde os testes são escritos antes do código ser implementado, conduzindo o processo de desenvolvimento.

## Veja Também
- [LuaUnit no GitHub](https://github.com/bluebird75/luaunit)
