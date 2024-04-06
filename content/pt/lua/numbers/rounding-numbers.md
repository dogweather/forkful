---
date: 2024-01-26 03:45:57.647203-07:00
description: "Como fazer: Lua n\xE3o inclui uma fun\xE7\xE3o de arredondamento pronta\
  \ como algumas outras linguagens. Historicamente, voc\xEA precisa escrever sua pr\xF3\
  pria fun\xE7\xE3o ou\u2026"
lastmod: '2024-04-05T21:53:47.049632-06:00'
model: gpt-4-0125-preview
summary: "Lua n\xE3o inclui uma fun\xE7\xE3o de arredondamento pronta como algumas\
  \ outras linguagens."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
```lua
-- O arredondamento básico em Lua não vem integrado, mas você pode definir uma função:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Para arredondar para um número específico de casas decimais:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Aprofundando
Lua não inclui uma função de arredondamento pronta como algumas outras linguagens. Historicamente, você precisa escrever sua própria função ou usar uma biblioteca de terceiros. Os recursos comuns dependem do `math.floor()` para arredondar para baixo e do `math.ceil()` para arredondar para cima, combinados com adicionar ou subtrair 0.5 antes de fazer isso, dependendo do sinal do número.

Alternativas para criar sua própria função incluem bibliotecas como "lua-users wiki" ou "Penlight". Cada uma tem seus benefícios e desvantagens, como recursos adicionais ou mais sobrecarga.

Internamente, essas funções normalmente funcionam explorando a maneira como os computadores armazenam números de ponto flutuante. Adicionar 0.5 a um float positivo que você deseja arredondar empurrará ele para além do limiar do próximo valor inteiro, então, quando você aplica o `math.floor()`, ele arredonda para baixo para esse inteiro mais próximo.

## Veja Também
- [Lua 5.4 Manual de Referência: As Funções Matemáticas](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Bibliotecas Lua Penlight: Matemática](https://github.com/lunarmodules/Penlight)
