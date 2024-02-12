---
title:                "Escrevendo testes"
aliases: - /pt/lua/writing-tests.md
date:                  2024-02-03T19:31:24.067493-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo testes"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever testes em programação envolve criar pequenas peças separadas de código para verificar automaticamente se diferentes partes da sua aplicação funcionam conforme esperado. Para programadores Lua, testar garante a confiabilidade e auxilia na manutenção da qualidade do código, acelerando o processo de depuração e tornando as modificações no código-base mais seguras.

## Como fazer:

Lua, sendo uma linguagem de script leve, porém poderosa, não inclui um framework de teste integrado. No entanto, bibliotecas de terceiros como Busted e LuaUnit tornam o teste relativamente direto. Aqui, vamos olhar para exemplos usando ambos.

### Usando Busted

Busted é um framework de teste Lua popular que oferece uma maneira flexível de escrever testes. Primeiramente, instale o Busted através do LuaRocks (gerenciador de pacotes do Lua) com `luarocks install busted`. Uma vez instalado, você pode escrever seus testes. Aqui está um teste simples para uma função `add` que soma dois números:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Função de adição", function()
  it("deve adicionar dois números corretamente", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Para executar os testes, execute `busted` no seu terminal. A saída de exemplo para um teste que passou deveria ser assim:

```
●
1 sucesso / 0 falhas / 0 erros / 0 pendentes : 0,002 segundos
```

### Usando LuaUnit

LuaUnit é outro framework de teste que segue as convenções xUnit e é fácil de configurar. Instale o LuaUnit via LuaRocks usando `luarocks install luaunit`. Aqui está como você poderia escrever um teste semelhante ao de cima com LuaUnit:

```lua
-- add.lua continua o mesmo

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Executando este script diretamente via Lua (`lua test_add.lua`) produzirá algo como:

```
.
Executou 1 teste em 0.001 segundos, 1 sucesso, 0 falhas
```

Tanto Busted quanto LuaUnit oferecem recursos extensivos para lidar com vários cenários de teste, incluindo mock, espionagem e teste assíncrono. A escolha entre eles reside nas necessidades específicas do seu projeto e na sua preferência pessoal em relação à sintaxe e funcionalidade.
