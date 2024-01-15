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

## Por que

Calcular datas no futuro ou passado é uma habilidade muito útil em programação, especialmente quando se lida com tarefas como cronogramas, lembretes e agendamentos. Com Swift, podemos facilmente realizar esses cálculos, tornando nosso código mais dinâmico e funcional.

## Como fazer

Para calcular uma data no futuro ou passado com Swift, precisamos usar a classe Date, que representa uma data específica. Vamos dar uma olhada em alguns exemplos de código para entender melhor como isso funciona.

```Swift
// Definindo a data atual como ponto de partida
let now = Date()

// Usando a função addingTimeInterval para adicionar um intervalo de tempo em segundos
let futureDate = now.addingTimeInterval(3600) // Adicionando 1 hora no futuro
let pastDate = now.addingTimeInterval(-3600) // Subtraindo 1 hora no passado
```

Agora, vamos imprimir as datas para ver o resultado:

```Swift
print("Data atual: \(now)")
print("Data no futuro: \(futureDate)")
print("Data no passado: \(pastDate)")
```

Ao executar esse código, teremos a seguinte saída:

```
Data atual: 2019-09-10 13:00:00 +0000
Data no futuro: 2019-09-10 14:00:00 +0000
Data no passado: 2019-09-10 12:00:00 +0000
```

Podemos ver que a data atual não foi alterada, mas a data no futuro foi incrementada em 1 hora e a data no passado foi subtraída em 1 hora.

Também podemos usar a função `date(byAdding:to:wrappingComponents:)` para adicionar componentes específicos em uma data, como dias, meses ou anos. Vamos dar uma olhada em um exemplo:

```Swift
// Definindo a data atual como ponto de partida
let now = Date()

// Definindo o calendário atual
let calendar = Calendar.current

// Adicionando 2 semanas no futuro
let futureDate = calendar.date(byAdding: .weekOfYear, value: 2, to: now) 
```

Ao executar esse código, teremos a seguinte saída:

```
Data atual: 2019-09-10 13:00:00 +0000
Data no futuro: 2019-09-24 13:00:00 +0000
```

Podemos ver que o valor de `value` foi definido como 2, o que resultou em adicionar 2 semanas no futuro.

## Mergulho Profundo

Além das funções mencionadas acima, o Swift também possui uma classe chamada DateComponents, que nos permite desmontar uma data e acessar seus componentes individuais, como dia, mês e ano. Isso pode ser útil quando precisamos realizar cálculos mais específicos.

Além disso, também podemos calcular a diferença entre duas datas usando a função `dateComponents(_:from:to:)`, que retorna um objeto DateComponents contendo os componentes de diferença entre as duas datas.

## Veja também

- Documentação oficial da Apple sobre Date: https://developer.apple.com/documentation/foundation/date
- Tutoriais da RayWenderlich sobre Swift: https://www.raywenderlich.com/swift