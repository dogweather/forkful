---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:59.702107-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Gerar números aleatórios é como jogar dados com o seu computador – você obtém um resultado imprevisível toda vez. Programadores usam essa técnica quando precisam de elementos de surpresa ou de amostras aleatórias para testar e simular cenários.

## Como Fazer:

```Ruby
# Gerando um número aleatório simples
puts rand(10) # Saída: um número entre 0 e 9

# Especificando um intervalo
puts rand(1..5) # Saída: um número entre 1 e 5

# Criando um objeto Random para mais controle
rng = Random.new
puts rng.rand(100) # Saída: um número entre 0 e 99
```

## Mergulho Profundo:

A geração de números aleatórios está por aí desde os primeiros dias da computação, importante para criptografia, jogos e simulação. No Ruby, a classe `Random` usa o algoritmo Mersenne Twister para criar sequências pseudoaleatórias - suficientemente imprevisíveis para a maioria dos usos, mas não para criptografia. Alternativas para segurança mais robusta incluem o uso da biblioteca `SecureRandom`.

Detalhando a implementação, `Random` pode ser semeado com um valor inicial (seed) para reproduzir uma sequência de números, útil para testes. A chamada `rand` sem argumentos retorna um número flutuante entre 0 e 1, enquanto `rand(10)` retorna um inteiro dentro do intervalo especificado.

```Ruby
# Usando seed para resultados previsíveis
s = Random.new(1234)
puts s.rand(100) # Saída sempre será a mesma com a mesma seed

# Gerando um número hexadecimal seguro
require 'securerandom'
puts SecureRandom.hex(10) # Saída: uma string hexadecimal aleatória de 20 caracteres

# Gerando um UUID
puts SecureRandom.uuid # Saída: um UUID versão 4 aleatório
```

## Veja Também:

- Documentação oficial do Ruby para a classe Random: [ruby-doc.org/core-2.7.0/Random.html](https://ruby-doc.org/core-2.7.0/Random.html)
- Documentação do SecureRandom para segurança adicional: [ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html)
- Um artigo sobre aleatoriedade e suas aplicações na criptografia: [Wikipedia sobre aleatoriedade](https://pt.wikipedia.org/wiki/Aleatoriedade)