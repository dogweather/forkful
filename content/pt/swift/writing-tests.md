---
title:                "Swift: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Swift é importante

Ao escrever código em Swift, é essencial testar o seu código para garantir que ele funcione corretamente. Escrever testes não apenas ajuda a encontrar e corrigir erros, mas também garante que o seu código continue funcionando conforme você adiciona novas funcionalidades. Além disso, testes bem escritos podem até mesmo servir como documentação para seu código.

## Como escrever testes em Swift

Para escrever testes confiáveis em Swift, você precisará usar o framework de teste XCTest. Ele é parte da biblioteca padrão da Apple e é fácil de aprender e usar. Aqui está um exemplo de um teste simples que verifica se uma função retorna o resultado correto:

```Swift
import XCTest
 
class MathTests: XCTestCase {
    func testAddition() {
        let result = Math.addNumbers(x: 5, y: 7)
        XCTAssertEqual(result, 12)
    }
}
```

No código acima, importamos o framework XCTest e criamos uma classe de teste que herda de XCTestCase. Dentro da classe, criamos uma função de teste com o prefixo "test" e usamos o método XCTAssertEqual para verificar se o resultado da função Math.addNumbers é igual a 12. Se a condição não for atendida, o teste falhará.

## Mergulho profundo em testes

Uma das melhores práticas ao escrever testes é ter um teste para cada cenário possível. Isso ajuda a cobrir todas as possíveis entradas e saídas do seu código e a garantir que ele não falhe em situações inesperadas. Também é importante testar tanto casos positivos (quando o código funciona como esperado) quanto casos negativos (quando o código não funciona como esperado) para ter certeza de que seu código está robusto.

Além disso, você pode usar os recursos do framework XCTest, como o XCTAssertThrowsError, para testar se uma função lança um erro corretamente. Isso é especialmente útil ao lidar com cenários de erro em seu código.

## Veja também

- [Documentação do XCTest](https://developer.apple.com/documentation/xctest)
- [Tutorial de introdução ao XCTest](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- [Práticas recomendadas para escrever testes em Swift](https://www.swiftbysundell.com/articles/unit-testing-best-practices-in-swift/)