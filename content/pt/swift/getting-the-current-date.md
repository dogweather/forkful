---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:44.897402-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Obter a data atual é pegar o dia, mês, ano, e hora em que um código está sendo executado. Programadores fazem isso para registros de log, funcionalidades de datas e várias outras operações dependentes do tempo atual.

## Como fazer:
```Swift
import Foundation

// Obtendo a data e hora atuais
let agora = Date()

// Formatando e exibindo a data e hora
let formatador = DateFormatter()
formatador.dateFormat = "dd/MM/yyyy HH:mm:ss"
let dataFormatada = formatador.string(from: agora)
print(dataFormatada) // Saída: a data e hora atuais, por exemplo: "31/03/2023 14:55:37"
```

## Mergulho Profundo
Antes de termos dispositivos com relógios internos, obter a data e a hora atual era mais complicado, muitas vezes dependendo de fontes externas. Hoje, em Swift, usamos a classe `Date` para capturar o instante atual, que conta os segundos desde o Unix Epoch (1 de janeiro de 1970). Alternativas incluem usar `Calendar` para lidar com fusos horários e localizações, ou `DateComponents` para acessar partes específicas da data.

Há uma coisa importante a se notar sobre o uso do `DateFormatter`: configurá-lo é considerado uma operação custosa. Portanto, se você estiver formatando muitas datas, é ideal reutilizar o mesmo `DateFormatter`, em vez de criar um novo a cada vez.

O horário de verão e diferentes calendários (gregoriano, budista, etc.) podem afetar como as datas são calculadas e exibidas, então, dependendo da sua aplicação, é preciso considerar esses fatores.

## Veja Também
- [Documentação oficial da classe Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial da Apple para trabalhar com datas e horas](https://developer.apple.com/documentation/foundation/datecomponents)
- [Guia rápido para DateFormatter](https://www.hackingwithswift.com/articles/140/the-complete-guide-to-dateformatter)
- [Stack Overflow: Exemplos de manipulação de data e hora em Swift](https://stackoverflow.com/questions/tagged/swift+date)
