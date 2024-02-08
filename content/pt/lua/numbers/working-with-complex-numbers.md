---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:43:07.933199-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Números complexos estendem a ideia da linha numérica unidimensional para o plano bidimensional, incluindo um eixo imaginário perpendicular. Programadores trabalham com eles em campos como processamento de sinais, dinâmica dos fluidos e engenharia elétrica, onde são essenciais para representar oscilações e outros fenômenos.

## Como fazer:
No Lua, você pode representar números complexos com tabelas. As operações básicas envolvem adicionar, subtrair, multiplicar e dividir essas tabelas. Veja como:

```lua
-- Define dois números complexos como tabelas
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Função para adicionar dois números complexos
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Saída de exemplo
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Mergulho Profundo
Os números complexos existem desde o século XVI, ajudando a resolver equações que não podiam ser resolvidas apenas com números reais. O Lua em si não possui um tipo embutido de número complexo. No entanto, isso não é um problema — você pode criar suas próprias manipulações de números complexos usando tabelas e funções, conforme mostrado acima. Ou, se suas necessidades são mais profundas, adquira uma biblioteca como LuaComplex. Esta é uma ótima escolha porque é construída especificamente para Lua e tira o trabalho manual das suas mãos. Bibliotecas como essa também otimizam frequentemente as operações internamente, então elas são mais rápidas do que fazer por conta própria.

## Veja Também
Para exemplos mais detalhados e operações avançadas, confira:

- Biblioteca LuaComplex: https://github.com/davidm/lua-complex
- Livro "Programando em Lua", para criação de tipo de dados customizados: https://www.lua.org/pil/11.1.html
- Wikipedia sobre usos dos números complexos em diferentes campos: https://en.wikipedia.org/wiki/Complex_number#Applications
