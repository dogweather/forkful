---
title:                "Convertendo uma string para letras minúsculas"
html_title:           "Lua: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Converter uma string para letras minúsculas é um processo importante no desenvolvimento de programas em Lua. Isso significa transformar todas as letras de uma string para sua versão em minúsculas. Os programadores fazem isso para garantir que a comparação de strings seja case-insensitive, ou seja, independente de maiúsculas e minúsculas.

## Como fazer:

Para converter uma string para minúsculas em Lua, podemos usar a função `string.lower()` junto com a string que queremos converter. Veja um exemplo de código abaixo:

```Lua
local str = "Exemplo De String"
print(string.lower(str))
```

A saída deste código será "exemplo de string", com todas as letras em minúsculas.

## Mergulho profundo:

A conversão de strings para minúsculas é uma técnica amplamente utilizada em programação. Isso se deve ao fato de que diferentes sistemas operacionais e linguagens de programação podem interpretar letras maiúsculas e minúsculas de maneiras diferentes. Ao converter todas as letras para minúsculas, garantimos que o programa seja mais portável e consistente em diferentes ambientes.

Além da função `string.lower()`, também é possível realizar a conversão utilizando a função `string.gsub()` em combinação com expressões regulares. No entanto, o uso da função `string.lower()` é mais simples e direto, e é a opção recomendada.

## Veja também:

Para mais informações sobre funções para manipulação de strings em Lua, recomendamos consultar a documentação oficial da linguagem. Além disso, é sempre bom estar familiarizado com diferentes métodos de manipulação de strings em outras linguagens, como Python e JavaScript, por exemplo. Isso pode ajudar a entender melhor os conceitos por trás da conversão de strings para minúsculas em Lua.