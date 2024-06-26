---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:57.401982-07:00
description: "Como fazer: O framework `Foundation` do Swift fornece a classe `Date`,\
  \ facilitando a obten\xE7\xE3o da data e hora atuais. Aqui est\xE1 um exemplo b\xE1\
  sico de como\u2026"
lastmod: '2024-03-13T22:44:46.931052-06:00'
model: gpt-4-0125-preview
summary: "O framework `Foundation` do Swift fornece a classe `Date`, facilitando a\
  \ obten\xE7\xE3o da data e hora atuais."
title: Obtendo a data atual
weight: 29
---

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
