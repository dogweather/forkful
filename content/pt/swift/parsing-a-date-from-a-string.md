---
title:                "Analisando uma data de uma string"
html_title:           "Swift: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porque?
Parsing de uma data de uma string significa extrair informações de uma data que está em um formato de texto legível para humanos, e transformá-la em um formato que o computador possa entender e manipular. Os programadores fazem isso para processar e armazenar datas de forma eficiente em seus aplicativos, permitindo que os usuários visualizem e manipulem dados temporais com precisão.

## Como fazer:
```Swift
// Criando uma string de data
let dataString = "25/10/2021"

// Definindo o formato da data
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

// Convertendo a string em uma data
if let data = dateFormatter.date(from: dataString) {
    // Imprimindo a data em um novo formato
    dateFormatter.dateFormat = "MMMM dd, yyyy"
    let novaDataString = dateFormatter.string(from: data)
    print(novaDataString)
}

// Output: "outubro 25, 2021"
```

## Mergulho Profundo:
Parsing de datas a partir de strings é uma tarefa comum e essencial na programação de aplicativos para lidar com dados temporais. Antes do advento de bibliotecas como a `DateFormatter` do Swift, os programadores precisavam escrever seu próprio código para analisar e formatar datas. Atualmente, existem também outras bibliotecas disponíveis para essa tarefa, como a `NSCalendar` e a `ISO8601DateFormatter`.

## Veja Também:
- [Documentação da DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Documentação da NSCalendar](https://developer.apple.com/documentation/foundation/nscalendar)
- [Documentação da ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)