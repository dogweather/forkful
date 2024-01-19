---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Eliminar caracteres que correspondem a um padrão é um processo em que certos caracteres em uma string são removidos com base em um padrão definido. Fazemos isso para limpar ou formatar dados, removendo caracteres indesejados ou desnecessários.

## Como se faz:
No Swift, podemos usar o método `removeAll` junto com um closure para combinar caracteres e removê-los. Aqui está um exemplo:

```Swift
var str = "Olá, Swift!"
str.removeAll(where: { $0 == "," || $0 == "!" })
print(str) // Imprime "Olá Swift"
```

Neste exemplo, estamos removendo as vírgulas e os pontos de exclamação da string. O resultado será "Olá Swift".

## Aprofundando
Historicamente, é comum ter que lidar com o gerenciamento de caracteres inapropriados ou desnecessários dentro de uma string. Swift, como muitas outras linguagens, possui uma maneira incorporada de lidar com isso.

Existem diversas alternativas em Swift para alcançar o mesmo efeito. Além do método `removeAll`, você pode usar o método `replacingOccurrences(of:with:)` para substituir padrões específicos de caracteres.

```Swift
var str = "Olá, Swift!"
str = str.replacingOccurrences(of: ",", with: "")
str = str.replacingOccurrences(of: "!", with: "")
print(str) // Imprime "Olá Swift"
```

Na prática, usar um método em vez de outro depende das circunstâncias e das suas necessidades específicas. Enquanto `removeAll` é útil para remover vários caracteres, `replacingOccurrences(of:with:)` é mais versátil e permite substituir caracteres, em vez de simplesmente removê-los.

## Veja Mais
- [Documentação oficial Swift - String](https://developer.apple.com/documentation/swift/string)
- [Exemplo de Swift Playground - Manipulação de String](https://developer.apple.com/documentation/swift/string)
- [Guia de programação do Swift - Trabalhando com strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)