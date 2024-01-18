---
title:                "Escrevendo testes"
html_title:           "Lua: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-tests.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Escrever testes é uma prática comum entre os programadores para garantir que seu código está funcionando corretamente. Isso envolve criar código adicional para verificar se o resultado esperado é retornado para uma determinada entrada de dados. É uma forma de melhorar a qualidade do código e evitar erros.

## Como fazer:

```Lua
-- Primeiro, precisamos importar a biblioteca de testes:
local test = require('test')

-- Em seguida, podemos definir uma função para testar:
function somar (a, b)
  return a + b
end

-- Agora, podemos escrever um teste usando o método `assert`:
test.assert(somar(2, 3) == 5) -- Isso deve retornar verdadeiro

-- Podemos também testar diversos casos com uma única função:
test.assert_all(
  somar(2, 3) == 5,
  somar(0, 0) == 0,
  somar(-5, 10) == 5
)

-- E se quisermos testar se uma função retorna um erro?
function dividir (a, b)
  return a / b
end

test.assert_error(dividir(4, 0)) -- Isso deve retornar verdadeiro
```

## Mergulho Profundo:

Escrever testes é uma prática recomendada por muitos desenvolvedores como forma de melhorar a confiabilidade do código. Isso ajuda a garantir que o código continue funcionando conforme ele é modificado ao longo do tempo. Existem outras formas de testar o código, como usar depuração ou realizar testes manuais, mas escrever testes automatizados é frequentemente considerado a maneira mais eficiente e precisa de garantir a corretude do código.

## Veja também:

- [Tutorial Lua para Iniciantes](https://www.lua.org/pil/contents.html)
- [Documentação Oficial do Módulo de Testes do Lua](https://www.lua.org/manual/5.3/manual.html#6.7)
- [Artigo sobre Testes Automatizados em Lua](https://www.oreilly.com/content/testing-automated-lua-tests/)