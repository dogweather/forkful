---
title:    "Swift: Calculando uma data no futuro ou passado"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que
Calcular datas no futuro ou passado é uma tarefa comum na programação Swift. Pode ser necessário para criar um calendário, marcar prazos ou agendar eventos. Aprender como calcular datas pode ser útil em uma variedade de projetos.

## Como Fazer
Para calcular datas em Swift, você pode usar a classe `Calendar` e seu método `date(byAdding:value:to:wrappingComponents)`. Vamos dar uma olhada em um exemplo simples que calcula a data de hoje mais um mês:

```
import Foundation

let hoje = Date() //cria uma data com o dia atual
let umMes = Calendar.current.date(byAdding: .month, value: 1, to: hoje) //adiciona 1 mês à data atual
```

No código acima, usamos a data atual, obtida com `Date()`, e o método `date(byAdding:value:to:wrappingComponents)` da classe `Calendar` para adicionar um mês à data atual. O primeiro argumento `month` indica que estamos adicionando um mês, o segundo argumento `1` indica a quantidade de meses que queremos adicionar e o terceiro argumento `hoje` é a data à qual queremos adicionar. O resultado é armazenado na constante `umMes` e pode ser utilizado em outras partes do código.

Outro exemplo seria calcular a data de ontem a partir da data atual:

```
import Foundation

let hoje = Date()
let ontem = Calendar.current.date(byAdding: .day, value: -1, to: hoje)
```

Neste caso, usamos o valor `-1` para indicar que queremos subtrair um dia da data atual.

## Mergulho Profundo
Além de adicionar valores a uma data, também é possível obter informações específicas de uma data. Para isso, podemos usar o método `component(_:from:)` da classe `Calendar`. Por exemplo, se quisermos obter o dia da semana de uma determinada data, podemos fazer isso da seguinte forma:

```
import Foundation

let data = Date()
let diaDaSemana = Calendar.current.component(.weekday, from: data)
```

Neste caso, usamos o método `component(_:from:)` e passamos o argumento `weekday` para obter o dia da semana. O resultado será um número correspondente ao dia da semana, por exemplo, 1 para domingo, 2 para segunda-feira, e assim por diante.

## Veja Também
- [Documentação do Swift sobre Manipulação de Datas](https://developer.apple.com/documentation/foundation/date)
- [Tutorial da Apple sobre Datas e Horários em Swift](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)