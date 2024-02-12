---
title:                "Obtendo a data atual"
aliases:
- pt/swift/getting-the-current-date.md
date:                  2024-02-03T19:10:57.401982-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual em Swift envolve o uso da classe `Date` para acessar a data e a hora em que o aplicativo está sendo executado. Os programadores precisam buscar a data atual por uma miríade de razões, que vão desde marcar eventos com timestamp, realizar cálculos de datas, até exibir datas e horários em uma interface de usuário.

## Como fazer:
O framework `Foundation` do Swift fornece a classe `Date`, facilitando a obtenção da data e hora atuais. Aqui está um exemplo básico de como obter a data atual:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Isso irá produzir algo como:

```
2023-04-12 07:46:23 +0000
```

O formato de saída segue o padrão ISO 8601, usando o fuso horário UTC. No entanto, você pode querer formatar essa data para fins de exibição. A classe `DateFormatter` do Swift vem em socorro:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Um exemplo de saída poderia ser:

```
12 de abril de 2023 às 10:46:23
```

Note que o formato de saída irá variar dependendo do local onde o dispositivo executando o código se encontra.

Para projetos que requerem manipulações de datas mais complexas, muitos desenvolvedores Swift recorrem a bibliotecas de terceiros, como a `SwiftDate`. Veja como você pode usar a `SwiftDate` para obter a data atual em um fuso horário e formato específicos:

Primeiro, adicione `SwiftDate` ao seu projeto usando SPM, CocoaPods, ou Carthage. Então:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Isso poderia produzir:

```
2023-04-12 09:46:23
```

Usando `SwiftDate`, você pode facilmente manipular datas e horários para diferentes fusos horários e locais, simplificando tarefas complexas de tratamento de datas em suas aplicações Swift.
