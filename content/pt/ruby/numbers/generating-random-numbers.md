---
date: 2024-01-27 20:34:54.698927-07:00
description: "Como Fazer: Ruby oferece v\xE1rios m\xE9todos para gerar n\xFAmeros\
  \ aleat\xF3rios, principalmente atrav\xE9s da classe `Random`. #."
lastmod: '2024-03-13T22:44:47.089512-06:00'
model: gpt-4-0125-preview
summary: "Ruby oferece v\xE1rios m\xE9todos para gerar n\xFAmeros aleat\xF3rios, principalmente\
  \ atrav\xE9s da classe `Random`."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como Fazer:
Ruby oferece vários métodos para gerar números aleatórios, principalmente através da classe `Random`.

### Número Aleatório Básico
Para gerar um número aleatório básico:

```Ruby
puts rand(10) # Gera um número aleatório entre 0 e 9
```

### Número Aleatório Dentro de um Intervalo
Para um número aleatório dentro de um intervalo específico:

```Ruby
puts rand(1..10) # Gera um número aleatório entre 1 e 10
```

### Usando a Classe Random
Para criar uma sequência repetível de números aleatórios, você pode usar a classe `Random` com uma semente.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Gera um número "aleatório" previsível
```

### Gerando um Elemento Aleatório de Array
Selecione um elemento aleatório de um array:

```Ruby
cores = ["vermelho", "azul", "verde", "amarelo"]
puts cores.sample # Seleciona aleatoriamente um elemento do array
```

### Saída de Exemplos:
Cada trecho de código acima, quando executado, produzirá saídas diferentes devido à sua natureza aleatória. Por exemplo, `rand(10)` pode resultar em `7`, enquanto `cores.sample` pode resultar em `"verde"`.

## Aprofundando
O conceito de gerar números aleatórios em ciência da computação é paradoxal, pois os computadores seguem instruções determinísticas. Métodos iniciais dependiam fortemente de entrada externa para alcançar imprevisibilidade. A aleatoriedade do Ruby é construída sobre o algoritmo Mersenne Twister, um gerador de números pseudoaleatórios conhecido por seu vasto período e distribuição uniforme, tornando-o altamente adequado para aplicações que requerem uma aleatoriedade de alta qualidade.

Embora os métodos integrados do Ruby sirvam bem à maioria das necessidades, eles podem não ser suficientes para todos os propósitos criptográficos, pois a previsibilidade dos números pseudoaleatórios pode ser uma vulnerabilidade. Para segurança criptográfica, os desenvolvedores Ruby podem explorar bibliotecas como `OpenSSL::Random`, que são projetadas para produzir números aleatórios criptograficamente seguros, garantindo maior imprevisibilidade para aplicações sensíveis.
