---
title:                "Swift: Comparando duas datas"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Swift?

Comparar datas é uma tarefa comum em muitos aplicativos, seja para verificar se um evento já ocorreu ou para ordenar itens em uma lista. Em Swift, existem várias maneiras de comparar duas datas e é importante entender os diferentes métodos para escolher o mais adequado para sua aplicação.

## Como comparar duas datas em Swift

Para comparar duas datas em Swift, existem três principais métodos: utilizando os operadores lógicos `>` e `<`, o método `compare()` da classe `Date` ou o método `compare(_:toGranularity:)` da classe `Calendar`.

### Exemplo 1: Utilizando operadores lógicos

```
let firstDate = Date()
let secondDate = Date(timeIntervalSinceNow: -3600) // 1 hora atrás
if firstDate > secondDate {
    print("A primeira data é mais recente que a segunda data.")
} else {
    print("A segunda data é mais recente que a primeira data.")
}
```
Saída:
```
A primeira data é mais recente que a segunda data.
```

### Exemplo 2: Utilizando o método compare()

```
let firstDate = Date()
let secondDate = Date(timeIntervalSinceNow: -3600) // 1 hora atrás
let result = firstDate.compare(secondDate)
if result == .orderedDescending {
    print("A primeira data é mais recente que a segunda data.")
} else if result == .orderedAscending {
    print("A segunda data é mais recente que a primeira data.")
} else {
    print("As datas são iguais.")
}
```
Saída:
```
A primeira data é mais recente que a segunda data.
```

### Exemplo 3: Utilizando o método compare(_:toGranularity:)

```
let firstDate = Date()
let secondDate = Date(timeIntervalSinceNow: -3600) // 1 hora atrás
let result = Calendar.current.compare(firstDate, to: secondDate, toGranularity: .day)
if result == .orderedDescending {
    print("A primeira data é mais recente que a segunda data.")
} else if result == .orderedAscending {
    print("A segunda data é mais recente que a primeira data.")
} else {
    print("As datas são iguais.")
}
```
Saída:
```
A primeira data é mais recente que a segunda data.
```

## Aprofundando na comparação de datas

Ao comparar datas, podemos considerar apenas a data ou também incluir a hora, minutos e segundos. O método `compare(_:toGranularity:)` nos permite especificar qual a granularidade desejada para a comparação: `.day` (dia), `.hour` (hora), `.minute` (minuto), `.second` (segundo) ou `.nanosecond` (nanossegundo).

Além disso, é importante lembrar que o retorno do método `compare()` ou `compare(_:toGranularity:)` é do tipo `ComparisonResult`, que pode ser `.orderedSame` (datas iguais), `.orderedAscending` (primeira data é anterior à segunda) ou `.orderedDescending` (primeira data é posterior à segunda).

## Veja também

- [Documentação oficial da classe `Date'](https://developer.apple.com/documentation/foundation/date)
- [Documentação oficial da classe `Calendar`](https://developer.apple.com/documentation/foundation/calendar)
- [Tutorial da Ray Wenderlich sobre manipulação de datas em Swift](https://www.raywenderlich.com/511-swift-tutorial-part-1-expressions-variables-and-constants)