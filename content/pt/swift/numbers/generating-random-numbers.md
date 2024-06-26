---
date: 2024-01-27 20:35:24.753946-07:00
description: "Como fazer: Swift oferece uma maneira simples de gerar n\xFAmeros aleat\xF3\
  rios atrav\xE9s de sua biblioteca padr\xE3o. Aqui est\xE1 como voc\xEA faz isso\
  \ para diferentes\u2026"
lastmod: '2024-03-13T22:44:46.916559-06:00'
model: gpt-4-0125-preview
summary: "Swift oferece uma maneira simples de gerar n\xFAmeros aleat\xF3rios atrav\xE9\
  s de sua biblioteca padr\xE3o."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como fazer:
Swift oferece uma maneira simples de gerar números aleatórios através de sua biblioteca padrão. Aqui está como você faz isso para diferentes tipos numéricos:

```Swift
// Gerar um inteiro aleatório entre 0 e Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Gerar um número de ponto flutuante aleatório entre 0.0 e 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Gerar um valor Bool aleatório
let randomBool = Bool.random()
print(randomBool)
```

A saída do exemplo pode variar porque, bem, estamos lidando com aleatoriedade afinal. Executar o código várias vezes produzirá números e valores booleanos diferentes.

## Aprofundando
A abordagem de Swift para a geração de números aleatórios é construída em cima de um gerador de números pseudoaleatórios (PRNG) robusto e eficiente. Antes do Swift 4.2, os desenvolvedores dependiam de bibliotecas externas ou das capacidades da plataforma subjacente, o que poderia levar a inconsistências entre diferentes plataformas e ambientes. Com a introdução de APIs nativas no Swift 4.2, gerar números aleatórios tornou-se tanto mais simples quanto mais consistente, independentemente da plataforma subjacente.

No entanto, é crucial entender que o gerador de números aleatórios padrão do Swift não é adequado para fins criptográficos. Para criptografia, os desenvolvedores devem usar o framework `Security` em plataformas da Apple, que fornece acesso a bytes aleatórios de forma criptograficamente segura. Até a minha última atualização, o Swift não inclui um gerador de números aleatórios criptográficos multiplataforma em sua biblioteca padrão, levando os desenvolvedores a procurarem bibliotecas de terceiros para essas necessidades em plataformas não Apple.

No âmbito da computação científica ou situações que exigem uma sequência determinística de números pseudoaleatórios (pela qual a sequência pode ser reproduzida exatamente), a geração de números aleatórios do Swift pode não ser a melhor opção sem a capacidade de semear o gerador. Nestes casos, bibliotecas e algoritmos especializados são frequentemente empregados para atender a esses requisitos precisos.
