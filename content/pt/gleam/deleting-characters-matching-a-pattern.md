---
title:                "Gleam: Excluindo caracteres que correspondam a um padrão"
simple_title:         "Excluindo caracteres que correspondam a um padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Às vezes, ao trabalhar com dados, precisamos limpar e formatar as informações que recebemos. Isso pode incluir a remoção de certos caracteres que seguem um determinado padrão. Esta é uma habilidade útil a se ter ao usar a linguagem de programação Gleam.

## Como fazer:

Para deletar caracteres que correspondem a um padrão em Gleam, podemos usar a função `String.replace`. Esta função recebe dois argumentos: a string original e o padrão que queremos substituir. Vamos ver um exemplo de como usar essa função:

```Gleam 
let original = "Hello, #Gleam! #programming"
let pattern = "#"
let output = String.replace(original, pattern)
```

O resultado seria `"Hello, Gleam! programming"`, já que a função `String.replace` removeu todos os caracteres que correspondiam ao padrão `#` da string original.

## Mergulho Profundo:

A função `String.replace` também pode receber um terceiro argumento opcional: o valor que queremos substituir no lugar dos caracteres correspondentes ao padrão. Se não fornecermos esse argumento, o padrão será simplesmente removido. Além disso, podemos usar expressões regulares para padrões mais complexos.

```Gleam
let original = "Hello, #Gleam! #programming"
let pattern = "#([a-z]+)"
let output = String.replace(original, pattern, "$1")
```

Neste exemplo, usamos uma expressão regular para encontrar apenas as hashtags seguidas por letras minúsculas e substituímos o padrão inteiro (hashtag + texto) pelo texto encontrado dentro dos parênteses. O resultado seria `"Hello, Gleam! programming"`, já que agora estamos substituindo apenas o texto após a hashtag.

## Veja também:

- [Documentação de String.replace em Gleam](https://gleam.run/documentation/api-reference/strings#replace)
- [Tutorial de expressões regulares em Gleam](https://medium.com/@jsjoeio/lets-learn-gleam-part-1-9c3f557f04d0)
- [Outros artigos sobre programação em Gleam](https://medium.com/tag/gleam)