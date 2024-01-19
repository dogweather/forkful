---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Obter a Data Atual em Swift

## O Que e Porquê?

Obter a data atual é uma tarefa comum em programação, usada para rastrear eventos de tempo real. É útil para coisas como registrar quando um evento ocorreu, para comparações de datas e muito mais.

## Como fazer:

Para obter a data atual em Swift, usamos a classe `Date` na biblioteca `Foundation`. Veja um exemplo de como fazer isto:

```Swift
import Foundation

let agora = Date()
print("A data e hora atual é \(agora)")
```

Quando você executa esse código, obterá uma saída semelhante a:

```Swift
A data e hora atual é 2022-07-27 14:33:30 +0000
```

## Aprofundando

Obter a data atual é uma função integrada na maioria das linguagens de programação desde os primórdios da programação, e Swift não é exceção. No entanto, `Date` em Swift representa um ponto específico no tempo, não uma data no sentido de dia, mês e ano.

Uma alternativa ao uso do `Date` diretamente é usar o `DateFormatter` para melhor controle do formato da data. Por exemplo:

```Swift
import Foundation

let agora = Date()

let formatador = DateFormatter()
formatador.dateFormat = "dd/MM/yyyy"
let dataFormatada = formatador.string(from: agora)

print("A data é \(dataFormatada)")
```

Isso imprimirá a data no formato "DD/MM/YYYY".

Por trás dos panos, Swift está usando a biblioteca `Cocoa` do Obj-C para acessar as funções do sistema operacional para obter a hora atual do sistema.

## Veja também:

Para mais informações sobre o trabalho com datas e horários em Swift, confira os seguintes recursos:

- Documentação oficial da Apple sobre a classe Date: https://developer.apple.com/documentation/foundation/date
- Tutorial sobre como formatar datas e horários em Swift: https://nsdateformatter.com/