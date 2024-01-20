---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Converter uma data em uma string consiste em transformar um objeto de data em uma sequência de caracteres. Os programadores fazem isso para apresentar datas de maneira humanamente compreensível ou para armazenar/formatar datas conforme necessário.

## Como fazer:
Para converter uma data em uma string no Swift, podemos usar a `DateFormatter`. Veja o exemplo abaixo com a saída de amostra.

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()

formatter.dateFormat = "dd.MM.yyyy"

let result = formatter.string(from: date)

print(result)
```

_Saída de amostra:_
```
"27.06.2022"
```

## Deep Dive
Historicamente, lidar com datas em linguagens de programação tem sido um desafio. Em Swift, temos `Date` e `DateFormatter` para tornar nosso trabalho mais fácil.

Existem outras maneiras de converter uma data em uma string em Swift - você poderia fazer uso de `String(describing:)` ou `"\(date)"`, por exemplo. No entanto, esses métodos não dão controle sobre o formato da string de data resultante.

A implementação do `DateFormatter` permite o uso facilitado de vários formatos de data e hora. Esta classe fornece métodos para conversão bidirecional entre datas e suas representações textuais.

## Veja também
- Apple Developer Documentation: [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- StackOverflow: [How to convert a Date into a String](https://stackoverflow.com/questions/35700281/date-format-in-swift)