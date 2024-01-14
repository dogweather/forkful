---
title:                "Swift: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string em Swift?

Capitalizar uma string é uma tarefa comum no desenvolvimento de aplicativos em Swift. Pode ser necessário para melhorar a legibilidade do texto ou para seguir padrões de escrita. Independentemente do motivo, é importante saber como capitalizar uma string corretamente.

## Como fazer:

Existem diferentes métodos para capitalizar uma string em Swift, dependendo do resultado desejado. Abaixo estão alguns exemplos usando o código Swift:

```
let palavra = "exemplo"
let primeiraLetra = String(palavra.prefix(1)).uppercased() 
// Output: "E"

let todasLetras = palavra.uppercased()
// Output: "EXEMPLO"

let primeiraMaiuscula = palavra.prefix(1).uppercased() + palavra.lowercased().dropFirst()
// Output: "Exemplo"

let camelCase = palavra.prefix(1).uppercased() + palavra.lowercased().dropFirst()
// Output: "exemplo"
```

## Mergulho Profundo:

Além dos métodos mencionados acima, existem outras formas de capitalizar uma string em Swift. Uma delas é usando a estrutura String.Protocol, que permite adicionar propriedades e métodos personalizados para strings. Outra opção é usar a função capitalize() para capitalizar apenas a primeira letra de cada palavra em uma string.

É importante mencionar que a formatação de letras maiúsculas e minúsculas pode variar de acordo com as regras gramaticais de diferentes idiomas. Portanto, é sempre recomendável verificar a documentação oficial da Apple antes de usar esses métodos em suas aplicações.

# Veja também:

- [Documentação oficial da Apple - String](https://developer.apple.com/documentation/swift/string)
- [Como capitalizar uma string em Swift](https://www.avanderlee.com/swift/capitalize-string-swift/)
- [Funcionalidades da estrutura String.Protocol](https://medium.com/flawless-app-stories/add-extensions-to-swift-existing-types-integers-strings-collections-protocol-1c1fc474540c)