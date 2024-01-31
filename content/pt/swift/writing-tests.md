---
title:                "Escrevendo testes"
date:                  2024-01-19
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (O Que e Por Que?)
Escrever testes é criar verificações automáticas para o seu código. Isso é feito para garantir que tudo funcione como esperado e para prevenir bugs quando alterações são feitas.

## How to: (Como fazer:)

Exemplo de Teste de Unidade usando XCTest:

```Swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {
    
    func testExample() {
        let result = YourApp.addTwoNumbers(numberOne: 2, numberTwo: 3)
        XCTAssertEqual(result, 5, "A soma de 2 + 3 deve ser 5")
    }
    
}

```

Saída esperada após rodar o teste:

``` 
Test Suite 'All tests' started at 2023-03-18 17:06:12.467
Test Suite 'YourAppTests' started at 2023-03-18 17:06:13.134
Test Case '-[YourAppTests testExample]' started.
Test Case '-[YourAppTests testExample]' passed (0.007 seconds).
```

## Deep Dive (Mergulho Profundo)

O XCTest é o framework de teste fornecido pela Apple, introduzido ao lado do Xcode 5 e iOS 7. Alternativas incluem Quick e Nimble para um estilo de descrição mais expressivo, mas XCTest é suficiente para a maioria dos casos. Internamente, escrever testes implica em entender asserções e o ciclo de vida do XCTestCase.

## See Also (Veja Também)

- Documentação XCTest da Apple: https://developer.apple.com/documentation/xctest
- Tutorial de testes em Swift: https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial
- Quick, uma lib de testes BDD para Swift: https://github.com/Quick/Quick
- Nimble, uma lib de asserções correspondente: https://github.com/Quick/Nimble
