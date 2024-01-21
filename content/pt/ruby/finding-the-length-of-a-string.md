---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:58.849875-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Encontrar o comprimento de uma string significa descobrir quantos caracteres ela contém. Programadores fazem isso para validar entradas, limitar texto, ou simplesmente para manipulação geral de dados.

## Como Fazer:
```Ruby
# Exemplo básico para encontrar o comprimento de uma string
frase = "Olá, mundo!"
comprimento = frase.length
puts comprimento  # Saída será 12

# Você também pode usar o método `size` que é um alias para `length`
tamanho = frase.size
puts tamanho      # Saída também será 12
```
## Mergulho Profundo
Historicamente, o método `length` existe desde as primeiras versões do Ruby, oferecendo um jeito intuitivo de se obter a extensão de uma string. Uma alternativa é o método `size`, que é exatamente igual ao `length`. Ambos retornam o número de caracteres `UTF-8` da string, o que é um detalhe importante: em Ruby, caracteres com acento ou emojis contam como um só caractere, graças à sua boa suporte à codificação `UTF-8`.

Um ponto de implementação é que estas funções efetivamente contam os elementos no array de caracteres da string, que é como strings são armazenadas em baixo nível.

## Veja Também:
- A documentação oficial do Ruby para [String#length](https://ruby-doc.org/core-3.1.0/String.html#method-i-length) e [String#size](https://ruby-doc.org/core-3.1.0/String.html#method-i-size).
- Um artigo prático sobre [codificação de caracteres em Ruby](https://www.justinweiss.com/articles/3-steps-to-fix-encoding-problems-in-ruby/).
- O livre online [Why's (Poignant) Guide to Ruby](https://poignant.guide/), para uma introdução divertida e não convencional ao Ruby.