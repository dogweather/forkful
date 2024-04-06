---
date: 2024-01-26 01:11:29.392040-07:00
description: "Como fazer: As fun\xE7\xF5es ficam mais complexas, lidando com v\xE1\
  rias tarefas."
lastmod: '2024-04-05T21:53:47.059950-06:00'
model: gpt-4-1106-preview
summary: "As fun\xE7\xF5es ficam mais complexas, lidando com v\xE1rias tarefas."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
```Lua
-- Define uma função simples para saudação
function saudar(nome)
    return "Olá, " .. nome .. "!"
end

-- Usa a função
print(saudar("Programador Lua")) -- Exemplo de Saída: Olá, Programador Lua!
```

As funções ficam mais complexas, lidando com várias tarefas:
```Lua
-- Uma função para calcular a área de um retângulo
function calcularArea(largura, altura)
    return largura * altura
end

-- Chama a função e imprime o resultado
local area = calcularArea(5, 4)
print(area)  -- Exemplo de Saída: 20
```

## Mergulho Profundo
Lua, desde o seu surgimento nos anos 90, tem incentivado o design modular. Organizar código com funções não é único de Lua—está em prática desde o amanhecer das linguagens de programação como Fortran e Lisp. Alternativas como código inline e copiar e colar o mesmo código várias vezes não são apenas mal vistas; são ninhos potenciais de bugs.

Em Lua, as funções são cidadãos de primeira classe, o que significa que podem ser armazenadas em variáveis, passadas como argumentos e retornadas de outras funções. São versáteis. A natureza single-threaded do Lua significa que você precisa manter as funções enxutas e eficientes em termos de desempenho. As funções podem ser locais (com escopo) ou globais, e entender quando usar cada uma pode fazer ou desfazer a eficiência do seu script.

## Veja Também
- Documentação oficial de Lua sobre funções: https://www.lua.org/pil/6.html
- Exemplos práticos do uso de funções em Lua: https://lua-users.org/wiki/SampleCode
- Práticas de código limpo em Lua: https://github.com/Olivine-Labs/lua-style-guide
