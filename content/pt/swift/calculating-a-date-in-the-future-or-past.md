---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Swift: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Calcular uma data no futuro ou no passado é uma forma de manipular datas em programação, permitindo identificar um ponto de tempo específico com base em uma data conhecida. Programadores fazem isso para gerenciamento de eventos, cálculos de prazos, agendamentos e aplicações similares.

## Como Fazer:
Utilizaremos a biblioteca `Foundation` e o objeto `DateComponents` para auxiliar na tarefa. Para códigos Swift, utilizaremos as tags ```Swift`.

```Swift
import Foundation

let agora = Date()

let calendario = Calendar.current

// Para calcular uma data no futuro
if let futura = calendario.date(byAdding: .month, value: 2, to: agora) {
    print("Daqui a dois meses será: \(futura)")
}

// Para calcular uma data no passado
if let passado = calendario.date(byAdding: .year, value: -1, to: agora) {
    print("Um ano atrás foi: \(passado)")
}
```

A saída será os respectivos dias, 2 meses adiante e 1 ano atrás, respectivamente.

## Mergulho Profundo
Nem sempre a manipulação de datas foi uma tarefa simples para programadores. Antigamente, muitos códigos manipulavam datas manualmente causando diversos problemas. Com o tempo, idiomas e bibliotecas evoluíram para fornecer formas mais precisas e fáceis para se trabalhar com datas e horários.

Swift oferece várias formas de manipular datas. Acima usamos `Calendar.date(byAdding:value:to:)` devido a sua simplicidade e funcionalidade robusta. Esta função cuida de todas as peculiaridades do calendário, como meses com números diferentes de dias e anos bissextos.

## Veja Também
Para aprofundar ainda mais os seus conhecimentos, vale a pena conferir estes links:
- [Date and Time Programming Guide](https://developer.apple.com/documentation/foundation/date_and_time_programming_guide): um guia abrangente da Apple sobre a programação de datas e horários.
- [Working with Dates in Swift](https://learnappmaking.com/dates-date-swift-how-to/): um exemplo detalhado de como trabalhar com datas em Swift.
- [Swift By Sundell](https://www.swiftbysundell.com/articles/dealing-with-date-and-time-in-swift/): um artigo interessante sobre a manipulação de datas e horários no Swift.