---
title:                "Encontrando o tamanho de uma string"
html_title:           "Ruby: Encontrando o tamanho de uma string"
simple_title:         "Encontrando o tamanho de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Encontrar o comprimento de uma string significa determinar o número de caracteres que ela contém. Os programadores frequentemente fazem isso para garantir que o texto se encaixe em uma determinada área, para validar entradas do usuário ou para manipular earmazenar dados.

## Como fazer:

Para encontrar o comprimento de uma string em Ruby, podemos usar o método `length` ou `size`. Veja os exemplos abaixo:

```Ruby
"Olá".length #=> 3
"Eu amo Ruby!".size #=> 12
```

## Mergulho Profundo:

O método `length` é uma abreviação do método `size`, mas eles são funcionalmente equivalentes. Na verdade, ambos invocam o método `size` por baixo dos panos. Outra alternativa é usar o operador `count`, que permite contar o número de ocorrências de caracteres específicos em uma string.

## Veja também:

- [Documentação do Ruby sobre os métodos length e size](https://ruby-doc.org/core-3.0.0/String.html#method-i-length)
- [Diferença entre os métodos size, length e count em Ruby](https://stackoverflow.com/questions/4050187/size-vs-length-vs-count-what-is-the-difference)