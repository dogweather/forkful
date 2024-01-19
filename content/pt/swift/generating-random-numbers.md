---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que e Porquê?
Gerar números aleatórios é o processo de produção de valores que não possuem previsibilidade. Programadores fazem isso principalmente para realizar testes em software ou implementar funções aleatórias.

## Como Fazer:
Aqui está um exemplo simples de como gerar um número aleatório no Swift usando a função `random(in:)`.

```Swift
let numeroAleatorio = Int.random(in: 0..<10)
print(numeroAleatorio)
```

Quando você executa esse código, verá um número aleatório entre 0 e 9 impresso no console.

## Mergulhando:
Swift moderno tornou a geração de números aleatórios muito fácil. No passado, era comum usar a função `arc4random()`, mas em Swift, isso é menos ideal pois produz um valor de UInt32, que pode ou não ser o que você precisa.

Existe a alternativa do `GameplayKit` framework que fornece diferentes técnicas de aleatorização, como a aleatorização de Gauss, permitindo mais complexidade e controle.

Contudo, em muitos casos, a função `random(in:)` integrada do Swift será suficiente. Ela usa uma nova técnica chamada "random Number Generator Protocol" que faz um trabalho melhor garantindo a aleatoriedade e a segurança dos números gerados.

## Veja Também:
Para aprender mais sobre números aleatórios em Swift, confira as seguintes fontes:

- Documentação oficial da Swift sobre números aleatórios: [Swift Random](https://developer.apple.com/documentation/swift/randomnumbergenerator) 
- Um artigo profundo sobre aleatoriedade no Swift: [Random Numbers in Swift](https://www.hackingwithswift.com/articles/136/how-to-generate-random-numbers-in-swift) 
- O protocolo do gerador de números aleatórios em Swift: [Random](https://developer.apple.com/documentation/swift/randomnumbergenerator)