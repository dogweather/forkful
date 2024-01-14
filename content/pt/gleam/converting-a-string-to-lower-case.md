---
title:                "Gleam: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador Gleam, provavelmente está sempre procurando maneiras eficientes de manipular strings. Converter uma string para letras minúsculas é uma tarefa comum em muitos projetos, e entender como fazer isso em Gleam pode economizar tempo e esforço.

## Como fazer

A função `String.to_lower` é usada para converter uma string para letras minúsculas. Veja um exemplo prático abaixo:

```Gleam
let string = "ESTE TEXTO SERÁ CONVERTIDO!"
let lower_case = String.to_lower(string)
```

O valor resultante da variável `lower_case` será "este texto será convertido!". Como você pode ver, todos os caracteres maiúsculos foram transformados em minúsculos.

Existem também funções para converter apenas a primeira letra de uma string em minúscula, como `String.lower_first` ou `String.capitalize`. Experimente e veja qual atende às suas necessidades específicas.

## Mergulho profundo

Você pode estar se perguntando como a função `String.to_lower` trabalha internamente. Basicamente, ela percorre cada caractere na string e usa a tabela ASCII para determinar se ele é uma letra maiúscula. Se for o caso, o caractere é modificado para sua versão minúscula. Esse processo é realizado para cada caractere da string até que ela seja completamente convertida em letras minúsculas.

É importante lembrar que, assim como a maioria das funções de string, a função `String.to_lower` retorna uma nova string e não modifica a string original. Isso significa que você precisará atribuir o resultado a uma nova variável.

## Veja também

- Documentação oficial sobre strings em Gleam: https://gleam.run/documentation/guides/strings/