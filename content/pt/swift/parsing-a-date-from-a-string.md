---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Lidando com Datas em Swift: Parsing de uma String para Date

## O que & Por quê?

Tratar datas é uma tarefa comum na programação. Fazer o parsing de uma data a partir de uma string significa converter uma representação textual de uma data em um objeto `Date` do Swift. Isso é feito para manipular melhor as informações de data na lógica de programação, facilitando cálculos e conversões.

## Como Fazer: 

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date = dateFormatter.date(from: "2022-01-01")

print(date)
```
 
Neste pequeno trecho de código acima, transformamos a string `"2022-01-01"` em um objeto `Date`. A saída será algo como `Optional(2022-01-01 00:00:00 +0000)`, expressando a data na forma padrão ISO 8601. 

## Imersão Profunda

O modelo de dados de data e tempo do Swift é inspirado em padrões de tempo Unix e ISO 8601. Esses padrões são amplamente aceitos e têm o benefício de serem conceitualmente simples e computacionalmente eficientes. 

Existem algumas alternativas para fazer o parser de uma string para uma data em Swift, incluindo bibliotecas externas como a ISO8601DateFormatter. Porém, muitas vezes a classe `DateFormatter` padrão é suficiente.

Pode ser útil saber que o `DateFormatter` do Swift usa internamente um `NSDateFormatter`, uma classe mais antiga disponível desde o Objective-C. Isso deixa claro como o Swift mantém a compatibilidade e constroi a partir da fundação do Objective-C.

## Veja Também

O Swift tem várias classes poderosas para lidar com datas, horários e fusos horários. Aqui estão alguns links para aprofundar seu conhecimento:

- [Date](https://developer.apple.com/documentation/foundation/date) — Objeto que encapsula a noção de um único ponto no tempo.
- [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter) — Converte entre datas e suas representações textuais.
- [Calendar](https://developer.apple.com/documentation/foundation/calendar) — Fornece maneiras de comparar datas e realizar operações de calendário baseadas em regras específicas de localização.
- [TimeZone](https://developer.apple.com/documentation/foundation/timezone) — Um valor que representa um fuso horário geográfico.