---
title:                "Utilizando expressões regulares"
html_title:           "Swift: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Swift?

Expressões regulares são uma ferramenta poderosa para manipulação de strings em qualquer linguagem de programação, incluindo Swift. Elas permitem que você procure e manipule padrões em um texto de maneira eficiente e precisa. Portanto, se você estiver lidando com strings em seu código Swift, aprender a utilizar expressões regulares pode tornar seu trabalho muito mais fácil e eficiente.

## Como usar Expressões Regulares em Swift

Para utilizar expressões regulares em seu código Swift, primeiro é necessário importar o framework `Foundation`. Em seguida, você precisará criar uma instância da classe `NSRegularExpression`, passando a expressão regular como um parâmetro. Por exemplo, se você quiser verificar se uma string contém apenas números, pode usar a seguinte expressão regular: `^[0-9]+$`.

```Swift
import Foundation

//Criando a instância da expressão regular
let regex = try! NSRegularExpression(pattern: "^[0-9]+$")

//String para ser verificada
let string = "1234"

//Verificando a string com a expressão regular
let isMatch = regex.matches(in: string, range: NSRange(string.startIndex..., in: string))

//Imprimindo o resultado
print(isMatch) //Saída: [1 match]
```

Excelente! Agora você sabe como utilizar expressões regulares em Swift. Mas, e se você quiser extrair uma parte específica do texto que corresponde ao padrão da expressão regular? Vamos ver como isso pode ser feito.

```Swift
import Foundation

//Criando a instância da expressão regular
let regex = try! NSRegularExpression(pattern: "[0-9]+")

//String para ser verificada
let string = "Alicia tem 25 anos"

//Procurando por números na string e extraindo-os
let matches = regex.matches(in: string, range: NSRange(string.startIndex..., in: string))

//Iterando pelos resultados e imprimindo-os
for match in matches {
    print(String(string[Range(match.range, in: string)!])) //Saída: 25
}
```

## Explore mais sobre Expressões Regulares em Swift

Aprender a utilizar expressões regulares em Swift é apenas o começo. Existem muitos recursos e recursos adicionais que podem ajudá-lo a se tornar um especialista em expressões regulares. Alguns tópicos que você pode explorar são:

- Sintaxe de expressões regulares em Swift
- Modificadores de expressão regular (como ignorar maiúsculas e minúsculas)
- Como substituir partes de uma string com expressões regulares
- Biblioteca de Expressão Regular Swift: [https://github.com/vermont42/RegularExpression](https://github.com/vermont42/RegularExpression)
- Documentação oficial do framework Foundation: [https://developer.apple.com/documentation/foundation/nsregularexpression](https://developer.apple.com/documentation/foundation/nsregularexpression)

## Veja também

- [Site da Apple sobre Expressões Regulares](https://developer.apple.com/library/archive/documentation/Foundation/Conceptual/NSRegularExpressionsTutorial/)
- [Tutorial de Expressões Regulares em Swift](https://www.raywenderlich.com/11580662-regular-expressions-tutorial-swift)
- [Vídeo sobre Expressões Regulares em Swift](https://www.youtube.com/watch?v=39fOPZl190U)