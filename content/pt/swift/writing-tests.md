---
title:                "Escrevendo testes"
html_title:           "Swift: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Swift?

Escrever testes pode ser uma tarefa tediosa e às vezes até parece desnecessária. Mas a verdade é que investir um tempo em escrever testes para o seu código pode trazer grandes vantagens. Além de garantir que seu código esteja funcionando corretamente, os testes também ajudam a identificar possíveis erros e a manter um código limpo e organizado.

## Como fazer?

Aqui vamos mostrar como escrever testes em Swift de maneira simples e eficiente. Primeiro, vamos criar uma classe de exemplo para testar. Dentro de uma função chamada `sum`, vamos adicionar dois números e retornar o resultado. Usando o operador de soma (`+`), nosso código ficaria assim:

```Swift
class Calculator {
    func sum(a: Int, b: Int) -> Int {
        return a + b
    }
}
```

Agora, vamos escrever alguns testes utilizando a estrutura de teste do Swift, chamada `XCTest`. Podemos criar uma função de teste para nossa função `sum`, utilizando a sintaxe `test[Nome da Função]`. Dentro dessa função, vamos chamar a função `sum` passando alguns valores e verificar se o resultado é o esperado utilizando `XCTAssertEqual`, que compara dois valores e sinaliza erro caso eles não sejam iguais.

```Swift
import XCTest

class CalculatorTests: XCTestCase {
    func testSum() {
        let calculator = Calculator()
        let result = calculator.sum(a: 2, b: 3)
        XCTAssertEqual(result, 5, "Resultado deveria ser igual a 5")
    }
}
```

Ao executar nossos testes, utilizando o atalho `cmd + U`, se tudo estiver correto, deveríamos ter uma mensagem de sucesso. Caso haja algum problema, o teste irá sinalizar qual foi a falha encontrada.

## Mergulho profundo

Escrever testes pode ser muito mais complexo do que apenas comparar resultados. Existem várias ferramentas e técnicas que podem ser utilizadas para melhorar a qualidade dos testes e garantir uma cobertura maior do seu código. Algumas delas incluem o uso de mocks e stubs, ciclos de testes automatizados e testes de unidade, integração e aceitação.

É importante também lembrar que testes unitários devem ser escritos para cada função ou método em seu código. Assim, é possível garantir que cada parte do código esteja funcionando corretamente e também facilita a identificação de falhas e erros.

## Veja também

- [Documentação oficial do XCTest](https://developer.apple.com/documentation/xctest)
- [Dica: Como escrever testes em Swift corretamente](https://medium.com/@areskiba/how-to-write-good-unit-tests-in-swift-7de1e7a153a)
- [Usando TDD em Swift](https://www.raywenderlich.com/566-swift-tdd-for-swift)