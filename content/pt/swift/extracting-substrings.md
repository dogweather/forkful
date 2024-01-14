---
title:    "Swift: Extraindo substrings"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings pode ser útil ao trabalhar com strings em Swift. Isso permite que você obtenha uma parte específica de uma string maior, o que pode facilitar o tratamento de dados e a manipulação de textos.

## Como fazer

Para extrair uma substring em Swift, você pode usar o método `suffix` ou `prefix` seguido do número de caracteres que deseja extrair. Por exemplo, para extrair os últimos 5 caracteres de uma string, você pode usar o seguinte código:

```Swift
let myString = "Hello, world!"
let newString = myString.suffix(5)
print(newString)
```

A saída deste código seria "rld!", pois foram extraídos os últimos 5 caracteres da string original.

## Aprofundando

Além de usar os métodos `prefix` e `suffix`, você também pode usar o método `range` para extrair uma substring específica de uma string. Este método requer o uso de `index` para indicar o início e o fim da substring que você deseja extrair. Por exemplo:

```Swift
let myString = "Hello, world!"
let newString = myString[4...9]
print(newString)
```

A saída seria "o, worl", pois foram extraídos os caracteres no índice 4 ("o") até o índice 9 ("l") da string original.

## Veja também

- [Documentação oficial da Apple sobre manipulação de strings em Swift](https://developer.apple.com/documentation/swift/strings_and_characters)
- [Tutorial em português sobre manipulação de strings em Swift](https://medium.com/@johnsundell/manipulac%CC%A7a%CC%83o-de-strings-com-swift-824ceddfba9d)
- [Outros métodos úteis para manipulação de strings em Swift](https://www.hackingwithswift.com/articles/141/8-useful-swift-extensions)