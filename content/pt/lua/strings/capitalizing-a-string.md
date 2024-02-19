---
aliases:
- /pt/lua/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:48.766397-07:00
description: "Capitalizar uma string envolve modificar o primeiro caractere de cada\
  \ palavra em uma frase para mai\xFAscula, garantindo que o restante esteja em min\xFA\
  scula.\u2026"
lastmod: 2024-02-18 23:08:58.266166
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve modificar o primeiro caractere de cada palavra\
  \ em uma frase para mai\xFAscula, garantindo que o restante esteja em min\xFAscula.\u2026"
title: Capitalizando uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string envolve modificar o primeiro caractere de cada palavra em uma frase para maiúscula, garantindo que o restante esteja em minúscula. Essa técnica é comumente usada para formatar texto para uma saída mais profissional ou legível, como preparar títulos ou entrada de usuário para exibição.

## Como Fazer:
Lua não possui uma função embutida para capitalizar strings, mas você pode facilmente realizar essa tarefa usando funções básicas de manipulação de strings. Aqui está uma função simples para capitalizar a primeira letra de uma única palavra:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Saída: Hello
```

Para capitalizar cada palavra em uma frase, você pode dividir a frase em palavras, capitalizar cada uma, e depois juntá-las novamente:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Saída: Hello World From Lua
```

Se você estiver trabalhando em um projeto onde o desempenho é crucial e você se encontrar precisando de capacidades de manipulação de string mais avançadas, considere usar uma biblioteca de terceiros como `Penlight`. Penlight melhora Lua com funções de manipulação de strings mais versáteis, entre outras utilidades:

```lua
-- Assumindo que Penlight está instalado:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Saída: Hello lua users

-- Nota: A função capitalized do Penlight só capitaliza a primeira palavra.
-- Para capitalizar cada palavra, você ainda implementaria uma solução personalizada ou exploraria outras bibliotecas.
```
