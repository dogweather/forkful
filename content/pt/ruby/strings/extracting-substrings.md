---
date: 2024-01-20 17:46:47.340647-07:00
description: "Extrair substrings \xE9 pegar peda\xE7os espec\xEDficos de uma string.\
  \ Fazemos isso para manipular, analisar ou alterar dados de forma eficiente e direcionada."
lastmod: '2024-03-13T22:44:47.082944-06:00'
model: gpt-4-1106-preview
summary: "Extrair substrings \xE9 pegar peda\xE7os espec\xEDficos de uma string. Fazemos\
  \ isso para manipular, analisar ou alterar dados de forma eficiente e direcionada."
title: Extraindo substrings
weight: 6
---

## What & Why?
Extrair substrings é pegar pedaços específicos de uma string. Fazemos isso para manipular, analisar ou alterar dados de forma eficiente e direcionada.

## How to:
Para extrair substrings em Ruby, você pode usar os métodos `slice`, `[]` e `slice!`. Aqui vão alguns exemplos práticos para você testar:

```Ruby
# String original
frase = "Ruby é uma pedra preciosa"

# Usando slice com intervalo de índices
sub_frase = frase.slice(0,4)
puts sub_frase  # Output: "Ruby"

# Usando colchetes com intervalo
sub_frase = frase[8,3]
puts sub_frase  # Output: "uma"

# Usando colchetes com índice único
letra = frase[5]
puts letra  # Output: "é"

# Usando slice! modifica a string original
frase.slice!(0..3)
puts frase  # Output: " é uma pedra preciosa"
```

## Deep Dive:
Desde que Ruby foi criado, por Yukihiro "Matz" Matsumoto, em meados dos anos 90, sempre houve maneiras eficientes de manipular strings. Extrair substrings é uma operação comum em muitas tarefas de programação, como processamento de texto ou dados de interações de usuário.

Os métodos `slice` e `[]` são basicamente intercambiáveis, mas `slice!` é mais destrutivo, pois altera a string original. Ao decidir qual método usar, pense no que você precisa: uma operação segura sem mudanças na string original? Ou uma maneira de modificar e reutilizar a string?

Ruby implementa esses métodos de forma a maximizar a eficiência, fazendo uso de recursos internos da linguagem, como ponteiros e manipulação de memória, para acessar rapidamente partes de strings.

Alternativas a esses métodos incluem expressões regulares, mas recomendamos começar com `slice` e `[]`, pois são mais simples de entender e suficientemente poderosos para a maioria das tarefas.

## See Also:
- Ruby documentation on String[slice]: https://ruby-doc.org/core-2.7.0/String.html#method-i-slice
- Ruby documentation on String[]: https://ruby-doc.org/core-2.7.0/String.html#method-i-5B-5D
- Um bom tutorial sobre expressões regulares em Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
