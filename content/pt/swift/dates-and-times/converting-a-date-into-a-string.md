---
date: 2024-01-20 17:37:37.452913-07:00
description: "Como Fazer: Sa\xEDda esperada pode parecer com isso, dependendo do momento\
  \ em que voc\xEA executa."
lastmod: '2024-04-05T21:53:47.283111-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda esperada pode parecer com isso, dependendo do momento em que voc\xEA\
  \ executa."
title: Convertendo uma data em uma string
weight: 28
---

## Como Fazer:
```swift
import Foundation

// Criar um objeto Date
let agora = Date()

// Configurar um DateFormatter
let formatador = DateFormatter()
formatador.dateFormat = "dd/MM/yyyy HH:mm"

// Converter a data para uma string
let dataComoString = formatador.string(from: agora)

// Exibir a string
print(dataComoString)
```

Saída esperada pode parecer com isso, dependendo do momento em que você executa:
```
"02/04/2023 15:41"
```

## Aprofundando
Na época do Objective-C, antes do Swift, os programadores já formatavam datas usando `NSDateFormatter`. Com o advento do Swift e suas atualizações, `DateFormatter` tornou-se mais swifty – mais seguro e mais fácil de usar devido ao sistema de tipos do Swift.

Alternativamente, você pode usar a classe `DateComponentsFormatter` para obter representações mais naturais do tempo passado, ou usar APIs de terceiros para suportar formatos mais complexos.

Internamente, `DateFormatter` depende das configurações locais do usuário; então, cuidado com diferenças de fuso horário e localidades. Especifique sempre o `locale` e `timeZone` quando trabalhando com datas que serão exibidas para usuários em regiões diferentes.

## Veja Também
- Documentação da Apple sobre DateFormatter: [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Guia do desenvolvedor da Apple para Trabalhar com Datas: [Working with Dates](https://developer.apple.com/documentation/foundation/dates_and_times)
- Tutorial sobre o uso de `DateComponents` e `Calendar`: [Date and Time Calculation](https://www.raywenderlich.com/5817-background-modes-tutorial-getting-started)
