---
title:                "Ruby: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que

A geração de números aleatórios é uma habilidade importante para qualquer programador, pois pode ser usada em diversos cenários, desde jogos até algoritmos de aprendizagem de máquina. Além disso, é uma maneira divertida de aprender mais sobre lógica de programação.

## Como fazer

Para gerar números aleatórios em Ruby, podemos usar o método `rand()`. Por exemplo, se quisermos gerar um número inteiro aleatório entre 1 e 10, podemos fazer:

```Ruby
rand(1..10)
```

Isso nos dará um número aleatório entre 1 e 10, incluindo ambos os extremos. Se quisermos gerar um número decimal aleatório, podemos usar `rand()` sem nenhum argumento:

```Ruby
rand()
```

Isso nos dará um número decimal aleatório entre 0 e 1. Podemos usar isso como base para gerar outros números aleatórios, por exemplo, se quisermos gerar um número decimal aleatório entre 100 e 200, podemos fazer:

```Ruby
100 + rand() * (200 - 100)
```

Podemos até mesmo gerar caracteres aleatórios, usando o método `sample()` em uma string contendo os caracteres que queremos. Por exemplo, se quisermos gerar uma senha aleatória com 8 caracteres, incluindo letras maiúsculas, minúsculas e números, podemos fazer:

```Ruby
("a".."z").to_a.concat(("A".."Z").to_a).concat((0..9).to_a).sample(8).join
```

Este código primeiro cria uma array contendo todas as letras maiúsculas, minúsculas e números, e então usa o método `sample()` para escolher aleatoriamente 8 elementos dessa array e os junta em uma string.

## Mergulho Profundo

Por trás dos panos, Ruby usa um algoritmo chamado "Mersenne Twister" para gerar números aleatórios. Este algoritmo é o padrão da linguagem e foi escolhido por ser rápido e ter uma alta qualidade nos números gerados. Entretanto, ele não é considerado seguro para aplicações que necessitam de criptografia, já que os números gerados podem ser repetidos. Para aplicações de segurança, é recomendado o uso de bibliotecas específicas para geração de números criptograficamente seguros.

## Veja também
- [Documentação oficial do método `rand()` em Ruby](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [Um artigo do blog sobre outras opções de geração de números aleatórios em Ruby](https://www.thoughtco.com/random-numbers-in-ruby-2908357)
- [Uma talk sobre a história e os algoritmos de geração de números aleatórios](https://www.youtube.com/watch?v=4Tq9xnCVJDo) (em inglês)