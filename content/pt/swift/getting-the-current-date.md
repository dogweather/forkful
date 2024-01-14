---
title:    "Swift: Obtendo a data atual."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante?

Em programação, muitas vezes precisamos ter acesso à data atual para realizar diferentes tarefas, como registrar o tempo de um evento, verificar se o usuário está usando o aplicativo no horário correto, entre outras situações. Portanto, obter a data atual é uma funcionalidade extremamente valiosa e útil em qualquer linguagem de programação, inclusive no Swift.

## Como obter a data atual em Swift?

Existem várias maneiras de se obter a data atual em Swift, mas iremos mostrar a maneira mais comum e simples, utilizando a classe `Date` e o `DateFormatter`. Confira abaixo um exemplo de código e a saída no console:

```Swift
// Importa a classe Foundation, que contém a classe Date
import Foundation

// Criação de uma instância de Date com a data e hora atual
let dataAtual = Date()

// Cria um objeto DateFormatter para exibir a data atual em formato padrão
let dateFormatter = DateFormatter()

// Define o estilo de formatação da data
dateFormatter.dateStyle = .long

// Exibe a data atual formatada
print(dateFormatter.string(from: dataAtual))

// Saída: 12 de dezembro de 2020
```

Portanto, utilizando a classe `Date` e o `DateFormatter`, conseguimos obter a data atual e exibi-la no formato desejado.

## Um mergulho mais profundo

Para aqueles que desejam explorar um pouco mais sobre como obter a data atual em Swift, é importante entender que a classe `Date` representa um ponto específico no tempo, e não apenas a data e hora atual. Além disso, é possível definir diferentes formatos de data utilizando o `DateFormatter`, como formato customizado, estilo de hora, fuso horário, entre outros.

Também é importante mencionar que a classe `Date` é baseada em UTC (Tempo Universal Coordenado) e, portanto, não leva em consideração o fuso horário local do dispositivo. Para tratar isso, recomenda-se utilizar a classe `TimeZone` em conjunto com o `DateFormatter`.

## Veja também

- [Documentação oficial do Swift - Date](https://developer.apple.com/documentation/foundation/date)
- [Documentação oficial do Swift - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Artigo sobre como trabalhar com datas em Swift](https://learnappmaking.com/format-dates-swift-how-to/)