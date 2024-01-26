---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Expressões regulares são padrões utilizados para encontrar e manipular strings de texto. Programadores as utilizam para validar dados, fazer buscas complexas e substituições em textos de maneira eficiente.

## How To:

```Swift
import Foundation

let texto = "Meu email é contato@exemplo.com.br"
let padraoEmail = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
if let range = texto.range(of: padraoEmail, options: .regularExpression) {
    let emailEncontrado = texto[range]
    print(emailEncontrado) // Saída: contato@exemplo.com.br
} else {
    print("Nenhum email válido encontrado.")
}
```

```Swift
let senha = "Senha123!"
let padraoSimplesSenha = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$"
if senha.range(of: padraoSimplesSenha, options: .regularExpression) != nil {
    print("Senha válida!") // Saída: Senha válida!
} else {
    print("Senha inválida.")
}
```

## Deep Dive

Expressões regulares, ou regex, têm suas raízes na teoria matemática dos autômatos e da linguagem formal, iniciada na década de 1950. Alternativas ao uso de regex incluem o processamento de strings com métodos como `contains`, `split`, e outras funções de manipulação. A implementação de regex no Swift é feita através do módulo `Foundation`, e embora poderosa, pode ter impactos significativos na performance se não utilizada corretamente.

## See Also

- Documentação oficial da Apple sobre o `NSRegularExpression`: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Tutorial da Ray Wenderlich sobre regex em Swift: [Regex Tutorial](https://www.raywenderlich.com/553-how-to-use-regular-expressions-in-swift)
- Livro "Mastering Swift" por Jon Hoffman, contendo um capítulo dedicado a regex em Swift.
