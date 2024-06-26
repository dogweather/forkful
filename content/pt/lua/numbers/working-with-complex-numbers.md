---
date: 2024-01-26 04:43:07.933199-07:00
description: "Como fazer: No Lua, voc\xEA pode representar n\xFAmeros complexos com\
  \ tabelas. As opera\xE7\xF5es b\xE1sicas envolvem adicionar, subtrair, multiplicar\
  \ e dividir essas\u2026"
lastmod: '2024-03-13T22:44:46.703206-06:00'
model: gpt-4-0125-preview
summary: "No Lua, voc\xEA pode representar n\xFAmeros complexos com tabelas."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

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
