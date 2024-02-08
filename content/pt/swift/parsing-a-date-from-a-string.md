---
title:                "Analisando uma data a partir de uma string"
aliases:
- pt/swift/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:36.201478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Que?
Analisar uma data a partir de uma string envolve converter representações textuais de data e hora em um objeto `Date`. Este processo é essencial em aplicações onde as datas são comunicadas como strings, como em respostas de API ou entradas de usuário, permitindo uma manipulação e formatação de datas mais fácil.

## Como Fazer:

### Usando o `DateFormatter` da Foundation
A biblioteca padrão do Swift, Foundation, fornece `DateFormatter` para converter strings em objetos `Date` e vice-versa. Para analisar uma data a partir de uma string, você especifica o formato da data que corresponde à string, e então usa o formatador para analisá-la.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Data analisada: \(date)")
} else {
    print("Falha ao analisar a data")
}
// Saída de Exemplo: Data analisada: 2023-04-29 22:00:00 +0000
```

Note que a saída pode variar com base no seu fuso horário.

### Usando ISO8601DateFormatter
Para formatos de data ISO 8601, o Swift fornece um formatador especializado, `ISO8601DateFormatter`, que simplifica o processo de análise.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Data ISO8601 analisada: \(date)")
} else {
    print("Falha ao analisar a data ISO8601")
}
// Saída de Exemplo: Data ISO8601 analisada: 2023-04-30 15:00:00 +0000
```

### Usando Uma Biblioteca de Terceiros: SwiftDate
Embora o Swift forneça ferramentas robustas para análise de datas, bibliotecas de terceiros como SwiftDate oferecem ainda mais flexibilidade e conveniência. Após adicionar a SwiftDate ao seu projeto, a análise se torna tão simples quanto:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Data analisada com SwiftDate: \(date)")
} else {
    print("Falha ao analisar data com SwiftDate")
}
// Saída de Exemplo: Data analisada com SwiftDate: 2023-04-30 00:00:00 +0000
```

A SwiftDate simplifica a análise com linguagem natural e uma ampla gama de formatos de data, tornando-a uma adição poderosa ao seu toolkit de programação Swift.
