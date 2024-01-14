---
title:    "Swift: Obtendo a data atual."
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Saber a data atual é uma tarefa essencial em muitos aplicativos. Ele pode ser usado para registrar eventos, mostrar atualizações em tempo real e muito mais. Neste artigo, explicaremos como obter a data atual usando Swift.

## Como fazer:

Primeiro, precisamos importar o framework "Foundation" do Swift para ter acesso aos recursos de data. Em seguida, podemos usar a função "Date()" para obter a data atual.

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Este código irá imprimir a data e hora atuais no console. No entanto, a saída será apresentada em um formato não legível para humanos. Para resolver isso, podemos usar a classe "DateFormatter" para formatar a data como desejamos.

```Swift
import Foundation

let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy" // Definindo o formato da data
let dateString = dateFormatter.string(from: currentDate)

print(dateString)
```

Neste exemplo, a data será formatada como "dd/MM/yyyy", mas você pode alterar a formato para o que for mais adequado às suas necessidades. Existem muitas opções de formatos disponíveis para data, como "MMM, yyyy" ou "HH:mm:ss", por exemplo.

## Mergulho profundo:

Além de obter a data atual, podemos utilizar a classe "Calendar" para acessar outras informações, como o dia da semana ou o fuso horário atual. Também podemos adicionar ou subtrair dias, meses ou anos da data atual usando a classe "DateComponents". Com um pouco de pesquisa e prática, é possível realizar várias tarefas com a data atual.

## Veja também:

- [Documentação oficial do Swift sobre data e hora](https://developer.apple.com/documentation/foundation/date)
- [Guia de formatação de datas em Swift](https://www.hackingwithswift.com/example-code/system/how-to-format-dates-with-dateformatter)
- [Tutorial prático sobre como utilizar a classe "Calendar" em Swift](https://medium.com/swift-india/working-with-date-and-time-in-swift-part-1-calendar-foundation-framework-4cb6df6f09b8)