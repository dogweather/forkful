---
title:                "Swift: Obtendo a data atual"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que obter a data atual é importante

Ao desenvolver um aplicativo, muitas vezes precisamos trabalhar com datas. Seja para exibir a data de criação de um post, agendar um evento ou simplesmente para exibir a data atual ao usuário, é essencial saber como obter a data correta em seu aplicativo. Felizmente, o Swift possui uma maneira fácil e eficiente de fazer isso.

## Como obter a data atual em Swift

Para obter a data atual em Swift, podemos usar a classe `Date`. Essa classe representa uma data e hora específicas em nosso código. Veja um exemplo simples de como podemos obtê-la:

````Swift
let dataAtual = Date()
print(dataAtual)
````

Isso irá imprimir a data e hora atual no formato padrão da sua localidade, como por exemplo `2020-09-20 22:30:00 +0000`. Se você quiser mostrar a data em um formato específico, podemos usar um `DateFormatter` para formatá-la. Veja um exemplo:

````Swift
let dataAtual = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let dataFormatada = formatter.string(from: dataAtual)
print(dataFormatada) // imprime 20/09/2020
````

## Mergulho Profundo

Além de simplesmente obter a data atual, podemos também realizar outras operações com a classe `Date`, como comparações entre datas, realizar cálculos e manipular seu formato. Também é importante considerar o uso de `Calendar` e `TimeZone` para manipular datas de forma mais precisa e personalizada. No entanto, é importante ter cuidado ao trabalhar com diferentes fusos horários e precisar de datas precisas em diferentes regiões.

# Veja também

- Documentação oficial da classe `Date` em Swift: https://developer.apple.com/documentation/foundation/date
- Como formatar datas em Swift: https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter
- Manipulando datas com `Calendar` e `TimeZone`: https://www.raywenderlich.com/6368-working-with-dates-in-swift-tutorial-for-dummies