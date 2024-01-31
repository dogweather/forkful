---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:38:31.485245-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Converter uma data de uma string significa transformá-la de texto para um objeto `Date` que o Swift pode entender e manipular. Fazemos isso para manipular datas em apps, fazer cálculos de tempo ou apenas para formatar de maneira adequada.

## Como Fazer:
```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy HH:mm"

if let date = dateFormatter.date(from: "27/03/2023 14:00") {
    print("A data é: \(date)")
} else {
    print("Houve um erro na conversão da data.")
}
```
Exemplo de saída:
```
A data é: 2023-03-27 17:00:00 +0000
```

## Mergulho Profundo
Antes do Swift e de `DateFormatter`, tínhamos que lidar com as datas de forma mais manual no Objective-C, o que era propenso a erros. Hoje, além do `DateFormatter`, temos alternativas como o `ISO8601DateFormatter` para um padrão de data internacional, e podemos até mesmo usar bibliotecas de terceiros que oferecem ainda mais flexibilidade. A implementação para parsing de data é crítica em muitos aplicativos, pois erros aqui podem levar a problemas sérios, como informações erradas sendo mostradas ao usuário ou até falhas na sincronização de dados.

O `DateFormatter` é uma classe que oferece ampla personalização, suportando diferentes fusos horários e idiomas, porém é importante notar que ela pode ser lenta se usada inadequadamente. Por exemplo, instanciá-la repetidas vezes ao invés de reusá-la pode causar problemas de performance. Por isso, é recomendado criar e configurar uma única instância de `DateFormatter` quando possível, especialmente em células de tabelas ou coleções que são reutilizadas frequentemente.

## Veja Também
- [Ray Wenderlich - Working with Date and Time in Swift](https://www.raywenderlich.com/5539282-working-with-date-and-time-in-swift)
