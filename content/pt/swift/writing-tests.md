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

## O que é e por que fazer?

Testes de código são uma prática comum entre programadores para garantir que os seus programas funcionem corretamente e sem erros. Eles são como uma garantia de qualidade para o seu código.

## Como fazer:

Para escrever testes em Swift, você pode criar uma classe separada com o sufixo `Tests`. Dentro dessa classe, você pode criar funções com o prefixo `test` para testar diferentes partes do seu código. Por exemplo:

```Swift
class MinhaClasseTests {

    func testSoma() {
        let resultado = soma(2, 3)
        XCTAssertEqual(resultado, 5)
    }

    func testMaiorQue() {
        let resultado = maiorQue(10, 5)
        XCTAssertTrue(resultado)
    }

    func testDivisao() {
        let resultado = divisao(10, 2)
        XCTAssertNotEqual(resultado, 3)
    }
}
```

O método `XCTAssertEqual` testa se o resultado de uma determinada expressão é igual ao valor esperado. O método `XCTAssertTrue` testa se o resultado de uma expressão é verdadeiro. E o método `XCTAssertNotEqual` testa se o resultado de uma expressão é diferente do valor esperado.

## Deep Dive:

Escrever testes não é uma prática nova e já é amplamente utilizado em outras linguagens de programação. Além dos métodos mencionados acima, o Swift também oferece outras opções para realizar testes, como o uso de mocks e stubs.

Uma alternativa ao uso de testes é fazer revisões de código em equipe. Isso pode ser útil para encontrar erros e problemas em outras partes do código que podem passar despercebidos durante os testes.

A implementação de testes em Swift é suportada pela ferramenta de linha de comando XCTest. Essa ferramenta é responsável por executar os testes e fornecer os resultados. Além disso, o Xcode também possui suporte integrado para criação e execução de testes.

## Veja também:

- [Documentação oficial do Swift sobre testes](https://swift.org/testing/)
- [Tutorial sobre testes em Swift](https://www.raywenderlich.com/71-ios-unit-testing-and-ui-testing-tutorial)
- [Vídeo explicando a importância dos testes em programação](https://www.youtube.com/watch?v=Eu35xM76kKY)