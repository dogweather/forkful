---
title:                "Gerando números aleatórios"
html_title:           "Ruby: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e por que? 
Gerar números aleatórios é um processo usado por programadores para criar valores aleatórios em seus códigos. Isso pode ser útil para simular jogos, testar algoritmos ou até mesmo criar senhas seguras.

## Como fazer:
Para gerar um número aleatório em Ruby, podemos usar o método `rand()`. Por exemplo, para gerar um número inteiro entre 1 e 10, podemos escrever:
```Ruby
rand(1..10)
```
Isso irá retornar um número aleatório entre 1 e 10. Também é possível gerar um número decimal usando `rand()`. Por exemplo, para gerar um número entre 0 e 1:
```Ruby
rand
```
A saída será um número decimal, por exemplo `0.52345`.

## Mergulho profundo:
Os geradores de números aleatórios têm uma longa história e são amplamente usados na matemática e ciência da computação. Existem também outras maneiras de gerar números aleatórios em Ruby, como o método `Random`. Além disso, é importante notar que esses números não são verdadeiramente aleatórios porque são gerados por algoritmos. No entanto, eles são suficientemente aleatórios para muitos propósitos.

## Veja também:
- Documentação oficial do método `rand()`: https://ruby-doc.org/core-2.7.4/Kernel.html#method-i-rand
- Outras maneiras de gerar números aleatórios em Ruby: https://www.rubyguides.com/2019/03/ruby-random/