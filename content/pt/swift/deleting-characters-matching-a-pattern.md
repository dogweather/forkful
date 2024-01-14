---
title:                "Swift: Excluindo caracteres que correspondem a um padrão"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Às vezes, em programação Swift, pode ser necessário excluir caracteres específicos dentro de uma string. Isso pode ser útil em casos como validação de entrada de usuário ou manipulação de dados.

## Como fazer
Você pode deletar caracteres que correspondem a um determinado padrão usando o método `replacingOccurrencies(of:with:)`. Veja um exemplo abaixo:

```Swift
let minhaString = "Este texto contém ##alguns# caracteres# de# teste"
let novoTexto = minhaString.replacingOccurrences(of: "#", with: "")

print(novoTexto)

// Saída:
"Este texto contém alguns caracteres de teste"
```

## Aprofundando
O método `replacingOccurrences(of:with:)` é muito útil, mas você também pode querer fazer uma exclusão mais específica, como excluir todos os dígitos de uma string. Para isso, você pode usar a classe `CharacterSet`. Vamos dar uma olhada em outro exemplo:

```Swift
let meuNumero = "+123 456-7890"
let setDeCaracteres = CharacterSet.decimalDigits
let novoNumero = meuNumero.components(separatedBy: setDeCaracteres).joined()
print(novoNumero)

// Saída:
"+ -"
```

Neste exemplo, usamos o método `components(separatedBy:)` para separar a string em componentes com base no conjunto de caracteres especificado. Em seguida, usamos o método `joined()` para juntar os componentes novamente sem os dígitos. Você pode experimentar com diferentes conjuntos de caracteres para excluir diferentes tipos de caracteres de uma string.

## Veja também
- [Documentação oficial do Swift sobre o método `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/swift/string/1786172-replacingoccurrences)
- [Documentação oficial do Swift sobre a classe `CharacterSet`](https://developer.apple.com/documentation/foundation/characterset)