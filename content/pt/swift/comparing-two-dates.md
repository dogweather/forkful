---
title:    "Swift: Comparando duas datas"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Por que comparar duas datas em Swift?

Ao desenvolver um aplicativo ou projeto em Swift, é comum a necessidade de comparar datas para determinar a lógica do programa ou simplesmente para exibir informações relevantes para o usuário. Portanto, entender como comparar duas datas de forma eficiente é uma habilidade importante para qualquer programador Swift.

# Como comparar duas datas em Swift?

Para comparar datas em Swift, podemos utilizar o tipo `Date` e seus métodos e propriedades. Vamos ver um exemplo prático de como comparar duas datas simples:

```
let date1 = Date()
let date2 = date1.addingTimeInterval(86400) // adiciona um dia (86400 segundos) a partir da data atual

if date1 < date2 { // compara se date1 é anterior a date2
    print("Date1 é anterior a date2")
} else if date1 > date2 { // compara se date1 é posterior a date2
    print("Date1 é posterior a date2")
} else { // caso nenhuma das opções anteriores seja verdadeira, significa que as datas são iguais
    print("Date1 e date2 são iguais")
}

// saída: Date1 é anterior a date2
```

Também podemos utilizar o método `compare()` para uma comparação mais detalhada, que retorna um enum `ComparisonResult` com os seguintes valores:

- `.orderedAscending`: quando o valor à esquerda é menor que o valor à direita
- `.orderedDescending`: quando o valor à esquerda é maior que o valor à direita
- `.orderedSame`: quando os valores são iguais

```
let result = date1.compare(date2)

switch result {
case .orderedAscending:
    print("Date1 é anterior a date2")
case .orderedDescending:
    print("Date1 é posterior a date2")
case .orderedSame:
    print("Date1 e date2 são iguais")
}

// saída: Date1 é anterior a date2
```

# Deep Dive: Comparando datas com precisão

Na seção anterior, utilizamos o método `compare()` para comparar duas datas em Swift. No entanto, esse método leva em consideração a precisão das datas apenas até o segundo. Se quisermos uma comparação mais precisa, podemos utilizar o método `compare(_:toGranularity:)`.

Este método leva em consideração a precisão especificada ao comparar as datas, levando em consideração variáveis como ano, mês, dia, hora, minuto e segundo. Podemos especificar a precisão desejada através da constante `Calendar.Component`, como por exemplo:

- `.year`: para comparar apenas o ano
- `.month`: para comparar ano e mês
- `.day`: para comparar ano, mês e dia
- `.hour`: para comparar ano, mês, dia e hora
- `.minute`: para comparar ano, mês, dia, hora e minuto
- `.second`: para comparar ano, mês, dia, hora, minuto e segundo

```
let date1 = Date()
let date2 = Date().addingTimeInterval(86400) // adiciona um dia (86400 segundos) a partir da data atual

let result = Calendar.current.compare(date1, to: date2, toGranularity: .day)

switch result {
case .orderedAscending:
    print("Date1 é anterior a date2")
case .orderedDescending:
    print("Date1 é posterior a date2")
case .orderedSame:
    print("Date1 e date2 são iguais")
}

// saída: Date1 é anterior a date2
```

# Veja também

- [Documentação oficial do Swift sobre o tipo `Date`](https://developer.apple.com/documentation/foundation/date)
- [Guia de estudo oficial do Swift](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)