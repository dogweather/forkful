---
date: 2024-01-26 03:45:57.647203-07:00
description: "Arredondar n\xFAmeros significa ajust\xE1-los ao inteiro mais pr\xF3\
  ximo ou a um determinado n\xFAmero de casas decimais. \xC9 um procedimento comum\
  \ na programa\xE7\xE3o para\u2026"
lastmod: '2024-03-13T22:44:46.704156-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros significa ajust\xE1-los ao inteiro mais pr\xF3ximo\
  \ ou a um determinado n\xFAmero de casas decimais."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## O Que & Por Quê?
Arredondar números significa ajustá-los ao inteiro mais próximo ou a um determinado número de casas decimais. É um procedimento comum na programação para reduzir a complexidade, melhorar o desempenho e para momentos em que a precisão além de um certo ponto não adiciona valor.

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
