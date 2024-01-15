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

## Por que

Gerar números aleatórios é uma ferramenta essencial em muitos programas de computador, pois permite criar uma variedade de resultados possíveis e imprevisíveis. Isso é útil em jogos, sorteios, testes e outras situações em que a aleatoriedade é desejada.

## Como fazer

Para gerar números aleatórios em Ruby, podemos usar o método `rand`, que retorna um número decimal aleatório entre 0 e 1. Para obter um número inteiro dentro de um intervalo específico, podemos multiplicar o resultado pelo tamanho do intervalo e somar o valor mínimo. Veja o exemplo abaixo:

```Ruby
# Gerando um número aleatório entre 1 e 10
rand(10) + 1
# Saída possível: 7

# Gerando um número aleatório entre 100 e 1000
rand(1000 - 100) + 100
# Saída possível: 539
```

Também podemos usar o método `rand` em conjunto com o operador de intervalo `..` para gerar uma sequência de números aleatórios. Veja o exemplo abaixo, em que geramos uma lista de 5 números aleatórios entre 1 e 100.

```Ruby
# Gerando uma lista de 5 números aleatórios entre 1 e 100
5.times { puts rand(1..100) }
# Saída possível:
# 75
# 12
# 92
# 35
# 50
```

## Mergulho profundo

É importante lembrar que, apesar de aparentemente aleatórios, os números gerados pelo `rand` são baseados em um algoritmo e, portanto, previsíveis. Se você quiser uma verdadeira aleatoriedade, pode usar o método `Random.rand`, que utiliza uma semente aleatória para gerar números. Veja o exemplo abaixo:

```Ruby
# Gerando um número aleatório com o método Random.rand
Random.rand(100)
# Saída possível: 75

# Gerando uma lista de 5 números aleatórios
# com o método Random.rand e uma semente diferente a cada execução
Random.new_seed
5.times { puts Random.rand(1..100, Random.new_seed) }
# Saída possível:
# 73
# 12
# 62
# 91
# 24
```

Além disso, podemos usar o método `srand` para definir uma semente específica e garantir que os números gerados sejam os mesmos a cada execução do programa. Veja o exemplo abaixo:

```Ruby
# Definindo uma semente específica
srand(12345)
# Gerando 3 números aleatórios com a mesma semente
3.times { puts rand(1..10) }
# Saída possível:
# 9
# 8
# 2
```

## Veja também

- [Documentação oficial de Ruby sobre o método `rand`](https://ruby-doc.org/core/Kernel.html#method-i-rand)
- [Documentação oficial de Ruby sobre o método `Random.rand`](https://ruby-doc.org/core/Random.html#method-c-rand)