---
title:                "Swift: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Swift?

Quando se trata de programação, é comum precisarmos comparar diferentes valores e objetos para realizar determinadas tarefas. No caso específico de datas, a comparação pode ser útil para verificar se uma data é anterior, posterior ou igual a outra data. Isso pode ser especialmente útil em aplicativos de gerenciamento de tarefas, agendas ou sistemas de reservas, onde é necessário verificar a data e hora de eventos ou prazos.

## Como comparar duas datas em Swift

Em Swift, para comparar duas datas, podemos utilizar o método `compare` da classe `Date`. Esse método retorna uma enumeração `ComparisonResult` com três possíveis valores: `.orderedAscending` (para quando a primeira data é anterior à segunda), `.orderedSame` (para quando as duas datas são iguais) e `.orderedDescending` (para quando a primeira data é posterior à segunda).

Podemos utilizar o método de comparação assim:

```swift
let data1 = Date()
let data2 = Date().addingTimeInterval(1000)

let resultado = data1.compare(data2)

switch resultado {
case .orderedAscending:
    print("A primeira data é anterior à segunda.")
case .orderedSame:
    print("As datas são iguais.")
case .orderedDescending:
    print("A primeira data é posterior à segunda.")
}
```

Nesse exemplo, definimos duas datas: `data1`, que é a data atual, e `data2`, que é a data atual com mais 1000 segundos adicionados. Depois, utilizamos o método `compare` para comparar essas duas datas e, com base no resultado, imprimimos uma mensagem em cada caso.

## Mais informações sobre comparação de datas em Swift

A classe `Date` também possui outros métodos e propriedades que podem ser úteis ao trabalhar com datas, como `addingTimeInterval` (para adicionar um determinado intervalo de tempo a uma data), `timeIntervalSince` (para obter o intervalo de tempo entre duas datas) e `timeIntervalSinceNow` (para obter o intervalo de tempo entre uma data e a data atual).

Além disso, em casos mais complexos, também é possível utilizar o framework `Calendar`, que oferece métodos e propriedades mais avançados para trabalhar com datas em diferentes calendários e fusos horários.

## Veja também

- [Documentação oficial do Swift sobre comparação de datas](https://developer.apple.com/documentation/foundation/date)
- [Comparación de fechas en Swift](https://www.aulasoftwarelibre.com/aprenderswift/es/posts/comparacion-fechas-swift-desde-html)
- [Fechas y horas en Swift: cómo usar las clases Date y Calendar](https://desarrolladores.es/fechas-y-horas-en-swift/)