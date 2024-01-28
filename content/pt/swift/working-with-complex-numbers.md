---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:45:49.161702-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Números complexos possuem uma parte real e uma imaginária (como 3 + 4i). Programadores os utilizam no Swift para tarefas como processamento de sinais, resolução de certos problemas matemáticos e simulação de física.

## Como fazer:
Swift não possui suporte embutido para números complexos, mas podemos criar o nosso próprio:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Métodos adicionais como subtração, multiplicação, etc.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Resultado: \(result.real) + \(result.imaginary)i")
// Saída de Exemplo: Resultado: 3.0 + 7.0i
```

## Mergulho Profundo
Números complexos surgiram no século 16 em equações algébricas. Eles são essenciais em mecânica quântica, teoria de controle e muitos outros campos. O Swift da Apple não possui uma biblioteca padrão para números complexos, diferentemente de linguagens como Python ou C++. Alternativas para criar o seu próprio incluem usar o pacote Numerics, que inclui suporte a números complexos, ou envolver a biblioteca complexa de C++ com a interoperabilidade do Swift.

## Veja Também
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
