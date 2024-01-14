---
title:                "Swift: Convertendo uma data em uma string."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Muitos aplicativos e programas precisam mostrar datas de forma legível para o usuário. Uma maneira de fazer isso é convertendo a data em uma string, que pode ser manipulada e formatada facilmente.

## Como converter uma data em uma string

Converter uma data em uma string em Swift é simples e envolve o uso do método `String(describing:)`. Veja um exemplo básico abaixo:

```Swift
import Foundation

let data = Date() // obtém a data atual
let stringData = String(describing: data) // converte a data em string
print(stringData) // imprime a string no console
```

A saída do código acima será algo como "2021-08-27 14:30:00 +0000", que pode ser facilmente lida e entendida pelos usuários.

Você também pode especificar o formato de data desejado usando a classe `DateFormatter`. Por exemplo, se deseja mostrar apenas o dia, mês e ano, você pode usar o seguinte código:

```Swift
let data = Date() // obtém a data atual
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy" // define o formato desejado
let stringData = dateFormatter.string(from: data) // converte a data em string com o formato especificado
print(stringData) // imprime a string no console
```

A saída do código acima será algo como "27/08/2021".

## Mais informações sobre a conversão de data em string

Ao converter uma data em uma string, é importante considerar o fuso horário e o formato da data. Além disso, é possível manipular e formatar a string resultante para melhor atender às necessidades do seu aplicativo ou programa.

Para obter mais informações sobre como trabalhar com datas em Swift, você pode consultar a documentação oficial da Apple [aqui](https://developer.apple.com/documentation/foundation/date). Também é possível encontrar vários tutoriais e exemplos na internet, como [este](https://www.simpleswiftguide.com/how-to-format-a-date-to-string-in-swift/).

## Veja também

- [Documentação da Apple sobre trabalhar com datas em Swift](https://developer.apple.com/documentation/foundation/date)
- [Tutorial sobre como formatar datas em string em Swift](https://www.simpleswiftguide.com/how-to-format-a-date-to-string-in-swift/)