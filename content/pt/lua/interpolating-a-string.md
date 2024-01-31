---
title:                "Interpolando uma string"
date:                  2024-01-20T17:51:08.674992-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Interpolar uma string é o ato de integrar variáveis e expressões dentro de uma string. Programadores fazem isso porque é uma maneira dinâmica e flexível de criar mensagens personalizadas e facilita a leitura e a manutenção do código.

## Como Fazer:
```Lua
-- Exemplo básico de interpolação de string usando a função string.format
local nome = "João"
local idade = 28
local mensagem = string.format("Olá, %s! Você tem %d anos.", nome, idade)
print(mensagem)  -- Saída: Olá, João! Você tem 28 anos.

-- Interpolação complexa com múltiplos tipos de dados
local preco = 49.99
local quantidade = 3
local frase = string.format("O total da sua compra de %d itens é R$ %.2f", quantidade, preco * quantidade)
print(frase)  -- Saída: O total da sua compra de 3 itens é R$ 149.97
```

## Aprofundamento
Historicamente, a interpolação de strings era comum em muitas linguagens, e no Lua, a função `string.format` é inspirada no `printf` da linguagem C. Em Lua, não há suporte nativo para a interpolação de string sintática, como você pode encontrar em Ruby ou Python com suas strings interpoladas ou "f-strings", respectivamente. No entanto, usar `string.format` é um método poderoso e flexível que permite não apenas inserir variáveis dentro de uma string, mas também formatar números e datas, por exemplo, com controle específico sobre a precisão e o formato de exibição. Alternativas incluem a concatenação manual de strings ou o uso de bibliotecas externas que podem oferecer uma sintaxe mais concisa para interpolação.
