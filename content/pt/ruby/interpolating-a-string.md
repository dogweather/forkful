---
title:                "Interpolando uma string."
html_title:           "Ruby: Interpolando uma string."
simple_title:         "Interpolando uma string."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Interpolação em strings é um recurso da linguagem Ruby que permite a inserção de variáveis ou expressões em uma string. Os programadores utilizam a interpolação para criar strings dinâmicas, tornando o código mais legível e fácil de modificar.

# Como fazer:

Para interpolar uma string em Ruby, é necessário usar as chaves (#{}) para envolver a variável ou expressão desejada. Veja o exemplo abaixo:

```Ruby
nome = "João"
puts "Olá, #{nome}!" 
```
## Deep Dive:

A interpolação em strings foi introduzida na linguagem Ruby em sua versão 1.9. Antes disso, os programadores precisavam utilizar a concatenação de strings para inserir variáveis em uma string. 

Existem algumas alternativas, como o método `sprintf` e a gem `erb`, mas a interpolação é considerada mais simples e eficiente. 

A string interpolada é convertida para uma instância da classe `String`, usando o método `#to_s`. Além disso, a interpolação também pode ser utilizada dentro de aspas simples (''), porém, neste caso, a variável ou expressão não será interpretada e será exibida como uma string literal.

## Veja também:

- [Documentação oficial do Ruby sobre interpolação](https://ruby-doc.org/core-2.7.1/doc/syntax/literals_rdoc.html#label-Percent+Strings)
- [Artigo sobre interpolação em strings no Medium](https://medium.com/@lucascaton/ruby-de-vez-em-quando-chamamos-de-interpola%C3%A7%C3%A3o-das-strings-c1b718bd7fbb)
- [Vídeo explicando sobre interpolação em strings no canal do Youtube "Programação Dinâmica"](https://www.youtube.com/watch?v=dKmS0OntjDE)