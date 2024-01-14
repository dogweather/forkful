---
title:    "Ruby: Gerando números aleatórios"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Gerar números aleatórios é uma habilidade essencial para qualquer programador, especialmente aqueles que trabalham com jogos, simulações ou criptografia. Isso permite criar um elemento de aleatoriedade em um programa, tornando-o mais dinâmico e interessante. Além disso, é uma ótima maneira de aprender e praticar as habilidades de programação.

## Como fazer isso em Ruby?

Gerar números aleatórios em Ruby é muito simples. Basta usar o método `rand()` e especificar o intervalo de números que você deseja. Por exemplo, para gerar um número aleatório entre 1 e 100, você pode usar o seguinte código:

```Ruby
numero_aleatorio = rand(1..100)
puts numero_aleatorio # Saída: 47
```

Você também pode usar `rand()` sem especificar um intervalo, o que irá gerar um número aleatório entre 0 e 1. Ou você pode usar `rand(n)`, onde `n` é um número inteiro, para gerar um número aleatório entre 0 e `n-1`.

Outra maneira de gerar números aleatórios em Ruby é usando a classe `Random`, que oferece mais controle e opções para gerar números aleatórios. Por exemplo:

```Ruby
numero_aleatorio = Random.new.rand(5...10)
puts numero_aleatorio # Saída: 7
```

## Mergulho profundo

A geração de números aleatórios pode parecer simples, mas há muito mais acontecendo nos bastidores do que você imagina. Quando usamos `rand()` ou `Random`, na verdade estamos usando um algoritmo computacional que é repetível e determinístico, o que significa que os mesmos números serão gerados a menos que você altere a "semente" ou ponto de partida.

Além disso, a aleatoriedade é um conceito complexo e muitas vezes difícil de alcançar em computação. Existem até mesmo bibliotecas especializadas em gerar números realmente aleatórios, como o `SecureRandom`, para serem usados em aplicações de criptografia.

## Veja também

- [Documentação do método `rand()`](https://ruby-doc.org/core/Kernel.html#method-i-rand)
- [Documentação da classe `Random`](https://ruby-doc.org/core/Random.html)
- [Documentação do módulo `SecureRandom`](https://ruby-doc.org/stdlib-2.6.1/libdoc/securerandom/rdoc/SecureRandom.html)