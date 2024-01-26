---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Capitalizar uma string significa transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para garantir que os nomes e títulos sigam as convenções tipográficas ou para melhorar a consistência e apresentação dos dados.

## Como Fazer:

Para capitalizar uma string em Ruby, você pode usar o método `capitalize` para simplesmente capitalizar a primeira letra da string, ou `titleize` (do Rails) ou `split.map(&:capitalize).join(' ')` para capitalizar cada palavra numa string. Aqui estão alguns exemplos:

```Ruby
frase = "isto é uma string em minúsculas"
puts frase.capitalize
# Saída: Isto é uma string em minúsculas

puts frase.split.map(&:capitalize).join(' ')
# Saída: Isto É Uma String Em Minúsculas

# Se você está usando Rails, pode ser ainda mais simples:
puts frase.titleize
# Saída: Isto É Uma String Em Minúsculas
```

## Mergulho Profundo:

Historicamente, em Ruby, o método `capitalize` existe desde as primeiras versões e serve para tornar maiúscula apenas a primeira letra de uma string. O método `titleize` é parte do ActiveSupport em Rails, que acrescenta várias utilidades para strings, entre outras coisas. Quanto à alternativa de usar `split.map(&:capitalize).join(' ')`, essa é uma forma padronizada de conseguir um efeito similar ao `titleize` sem depender do Rails.

Sobre o desempenho, é importante saber que operações de string podem ser custosas, principalmente em strings muito grandes ou em operações repetidas muitas vezes. O `capitalize` nativo tende a ser mais rápido do que as alternativas, pois está otimizado em C, a linguagem de implementação do Ruby. O `titleize` pode ser um pouco mais devagar, já que faz mais trabalho.

## Veja Também:

- Ruby Docs para o método `capitalize`: [String#capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- Rails Docs para o método `titleize`: [Inflector#titleize](https://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-titleize)
