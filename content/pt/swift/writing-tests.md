---
title:    "Swift: Escrevendo testes"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Swift?

Escrever testes é uma parte fundamental do processo de desenvolvimento de software em Swift. Testes bem escritos garantem que o código funcione conforme o esperado e ajuda a prevenir bugs no futuro. Além disso, testes permitem que o desenvolvedor tenha uma maior confiança em seu código e facilitem a identificação de erros durante o desenvolvimento.

## Como escrever testes em Swift

Para escrever testes efetivos em Swift, é necessário conhecer o módulo de teste XCTest da linguagem. Este módulo permite a criação de testes unitários e de integração para as suas aplicações em Swift. Abaixo está um exemplo de como escrever um teste simples de adição de dois números:

```Swift
func testSum() {

    // Given
    let a = 5
    let b = 10

    // When
    let result = a + b

    // Then
    XCTAssertEqual(result, 15, "A soma deve ser igual a 15.")
}
```

No código acima, definimos duas variáveis "a" e "b" com os valores que queremos somar. Em seguida, somamos essas variáveis e testamos se o resultado é igual a 15, que é o valor esperado. O método "XCTAssertEqual" verifica se os valores são iguais e caso não sejam, exibe a mensagem de erro.

## Funções mais complexas e testes de borda

Além de testes simples, o XCTest também suporta funções mais complexas e testes de borda. Por exemplo, se estivermos criando uma função que retorna o maior valor de um array de números, podemos escrever um teste utilizando o método "XCTAssertGreaterThan" para verificar se o maior valor retornado é realmente maior do que os outros valores do array.

Outra funcionalidade importante oferecida pelo XCTest é a possibilidade de testar erros com o método "XCTAssertThrowsError". Isso permite que o desenvolvedor crie testes para erros específicos que possam ocorrer em seu código.

## Veja também

- [Documentação oficial do XCTest](https://developer.apple.com/documentation/xctest)
- [Tutorial de testes com Swift](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- [Exemplos de testes em Swift](https://github.com/apple/swift/tree/master/test)

### See Also

- [Guia da Apple para testes em Swift](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/04-writing_tests.html)
- [Tutorial de testes unitários em Swift](https://www.appcoda.com/unit-testing-swift/)
- [Boas práticas para escrever testes em Swift](https://www.hackingwithswift.com/articles/108/how-to-write-better-unit-tests-in-swift)