---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Extrair substrings é o processo de obter uma parte específica de uma string. Programadores fazem isso quando precisam usar ou analisar parte de uma string sem alterar a original.

## Como fazer:

```Lua
-- Código Lua para extrair substring
str = "Olá Mundo Lua!"
substr = string.sub(str, 5, 9)

print(substr) -- Resultado: "Mundo"
```

Neste exemplo, a função `string.sub` é usada para extrair a substring da posição 5 à posição 9 da string `str`.

## Mergulho Profundo

A função `string.sub` atual em Lua é bastante simples de entender, mas nem sempre foi assim. Nas versões mais antigas do Lua, a função `string.sub` funcionava de maneira um pouco diferente. 

Como alternativa, poderíamos usar a função `string.find` para buscar a posição de uma substring específica e depois extrair usando `string.sub`. No entanto, isso é menos direto e geralmente mais lento.

Lua, sendo uma linguagem de script, tem um desempenho menos otimizado em relação às operações de string em comparação com linguagens como C ou Java. Assim, especialmente para operações de string longas e complexas, é aconselhável considerar outras estratégias ou talvez até outras linguagens.

## Veja Também

1. Documentação oficial Lua sobre a função `string.sub`: http://www.lua.org/manual/5.3/manual.html#pdf-string.sub
2. Para mais exemplos e uma explicação mais detalhada de como as strings são manipuladas no Lua, consulte: https://www.tutorialspoint.com/lua/lua_strings.htm
3. Uma discussão sobre a otimização de strings no Lua pode ser encontrada aqui: https://stackoverflow.com/questions/270073/lua-string-manipulation-performance