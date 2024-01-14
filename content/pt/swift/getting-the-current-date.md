---
title:                "Swift: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Existem várias razões pelas quais é importante poder obter a data atual em um aplicativo de programação. Pode ser necessário para registrar informações em um banco de dados, para exibir a data atual em um aplicativo ou para gerar relatórios com base nas datas atuais.

## Como fazer:

```Swift
//Usando a classe Date para obter a data atual
let dataAtual = Date()

//Formatando a data atual em um formato legível para o usuário
let formatoData = DateFormatter()
formatoData.dateFormat = "dd/MM/yyyy"
let dataFormatada = formatoData.string(from: dataAtual)

//Imprimindo a data formatada
print("A data atual é: \(dataFormatada)")

//Saída: A data atual é: 30/04/2021
```

## Mergulho Profundo:

Obter a data atual em Swift é relativamente simples, usando a classe `Date`. Esta classe representa uma data e hora específica, independentemente do fuso horário ou calendário utilizado. Além disso, é possível formatar a data de acordo com suas necessidades, definindo o formato desejado usando a classe `DateFormatter`.

## Veja também:

- [Documentação oficial do Swift sobre a classe Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial sobre como obter a data atual em Swift](https://ioscreator.com/tutorials/swift-how-to-get-the-current-date-time/)
- [Explicação mais detalhada sobre a classe Date em Swift](https://www.hackingwithswift.com/example-code/language/how-to-create-resume-dates-using-closed-ranges)