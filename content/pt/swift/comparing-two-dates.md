---
title:                "Comparando duas datas"
html_title:           "Swift: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que
Comparar duas datas é uma tarefa comum quando se trabalha com programação. É uma forma de verificar se uma data é anterior, posterior ou igual a outra, o que pode ser útil em muitos casos, como em aplicações de calendário, sistemas de reserva e gerenciamento de tarefas.

## Como Fazer
Para comparar duas datas em Swift, é necessário seguir alguns passos simples:

1. Crie duas variáveis do tipo `Date`.
2. Utilize o método `compare()` para comparar as datas. Este método retorna um `ComparisonResult`, que pode ser `orderedAscending` (data1 é anterior a data2), `orderedDescending` (data1 é posterior a data2) ou `orderedSame` (datas são iguais).
3. Caso prefira, você pode utilizar operadores de comparação como `<`, `>`, `==` para fazer comparações mais simples.

Um exemplo de código para comparar duas datas seria:

```Swift
let data1 = Date()
let data2 = Date(timeIntervalSinceNow: -86400) // cria uma data com 24 horas de diferença da data1

if data1.compare(data2) == .orderedDescending {
    print("A data1 é posterior à data2.")
} else if data1 > data2 {
    print("A data1 é posterior à data2.")
} else if data1 == date2 {
    print("As datas são iguais.")
} else {
    print("A data1 é anterior à data2.")
}
```

O resultado desse código seria "A data1 é posterior à data2".

## Deep Dive
Em Swift, as comparações entre datas são feitas com base no número de segundos que se passaram desde 1 de janeiro de 1970. Isso é conhecido como o Unix timestamp. A data atual é representada como um número muito grande de segundos desde essa data, permitindo assim realizar comparações com precisão. Além disso, o tipo `Date` também leva em consideração a localização do dispositivo, para que as comparações sejam feitas de acordo com o fuso horário correto.

É importante lembrar que o método `compare()` também pode ser utilizado com outras unidades de tempo, como anos, meses e dias, e até mesmo com outras grandezas não relacionadas à data, como números inteiros ou decimais. Essa flexibilidade é o que torna o tipo `Date` tão útil e versátil.

## Veja também
Aqui estão alguns recursos que podem ser úteis para complementar o seu conhecimento sobre como comparar datas em Swift:

- [Documentação da Apple sobre o tipo `Date`](https://developer.apple.com/documentation/foundation/date)
- [Tutorial sobre datas em Swift no site Ray Wenderlich](https://www.raywenderlich.com/5895-date-components-and-calendar-in-swift)
- [Projeto open-source com uma biblioteca de extensões para a classe `Date`](https://github.com/malcommac/SwiftDate)