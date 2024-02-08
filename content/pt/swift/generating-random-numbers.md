---
title:                "Geração de números aleatórios"
aliases:
- pt/swift/generating-random-numbers.md
date:                  2024-01-27T20:35:24.753946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios na programação é sobre criar valores numéricos não determinísticos ou imprevisíveis. Programadores usam números aleatórios por uma variedade de motivos, como simular imprevisibilidade em jogos, selecionar amostras aleatórias de conjuntos de dados ou para fins criptográficos.

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
