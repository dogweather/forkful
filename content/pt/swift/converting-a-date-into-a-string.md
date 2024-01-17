---
title:                "Convertendo uma data em uma string"
html_title:           "Swift: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que é e Por quê?

Converter uma data em uma string é um processo comum em programação em Swift. Isso significa transformar uma data em um formato de texto legível, que pode ser usado para exibição ou armazenamento. Programadores fazem isso para tornar as datas mais compreensíveis para os usuários ou para facilitar a manipulação de dados.

## Como fazer:

```Swift
let data = Date()

let formato = DateFormatter()
formato.dateFormat = "dd/MM/yyyy"

let dataString = formato.string(from: data)

print(dataString)

// Saída no console: "15/08/2020"
```

Neste exemplo, estamos usando a classe `DateFormatter` para definir um formato personalizado para a data e, em seguida, usando o método `string(from:)` para converter a data em uma string de acordo com esse formato. Esta é uma das maneiras mais simples de converter uma data em uma string.

## Mergulhando mais fundo:

Ao longo da história da programação, diferentes linguagens e frameworks tiveram suas próprias maneiras de converter datas em strings. Em Swift, o uso da classe `DateFormatter` é considerado a melhor prática para converter datas em strings. No entanto, também é possível usar a função `String(describing:)`, que retornará uma representação de texto de qualquer valor, incluindo uma data.

## Veja também:

- Documentação oficial da classe `DateFormatter` em [developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Documentação oficial da função `String(describing:)` em [developer.apple.com/documentation/swift/string/initfromarbitrarysubject](https://developer.apple.com/documentation/swift/string/initfromarbitrarysubject)