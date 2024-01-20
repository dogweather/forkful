---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Gerando números aleatórios com Ruby

## O que & por quê?

Gerar números aleatórios é o processo de criação de números de uma maneira não previsível. Nos programas, isso é útil para criar aleatoriedade, como embaralhar um baralho de cartas em um jogo ou selecionar um item aleatório de uma lista.

## Como fazer:

Aqui está um exemplo de como gerar um número aleatório em Ruby.

```Ruby
puts rand(100) # Gera um número entre 0 e 99
```

E aqui está um exemplo de como utilizar a semente (seed) para gerar sequências aleatórias previsíveis:

```Ruby
srand 1234      # Define a semente
puts rand(100)  # Saída: 47
srand 1234      # Redefine a semente
puts rand(100)  # Saída: 47 (a mesma saída)
```

## Deep Dive

Historicamente, os geradores de números aleatórios eram dispositivos físicos, como dados ou rodas giratórias. Hoje, utilizamos algoritmos matemáticos para fazer essa tarefa.

Alternativas para `rand` em Ruby incluem a Classe `Random`, que fornece uma maneira de gerar números aleatórios que são mais seguros para uso em aplicações criptográficas.

```Ruby
random = Random.new
puts random.rand(100) # Gera um número entre 0 e 99
```

O método `rand` gera números aleatórios usando um algoritmo chamado "Mersenne Twister", que é conhecido por produzir sequências de números aleatórios com grandes períodos.

## Veja Também

- Documentação oficial do Ruby sobre a classe Random: [https://ruby-doc.org/core-2.7.0/Random.html]
- 'Math.random vs Random.new' em StackOverflow: [https://stackoverflow.com/questions/19861702/ruby-random-vs-math-rand]
- Artigo sobre 'Mersenne Twister' na Wikipedia: [https://pt.wikipedia.org/wiki/Mersenne_twister]

Por fim, lembre-se de experimentar e explorar essas ferramentas ao máximo. A mistura de prática e teoria é a melhor forma de aprender a programar.