---
title:                "Convertendo uma data em uma string"
html_title:           "Swift: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, é necessário converter uma data em uma string para apresentar informações de forma mais legível ou para armazenar em um formato específico. Isso pode ser útil em aplicativos que envolvem agendamentos, históricos de transações ou simplesmente para fins de exibição.

## Como fazer

Você pode facilmente converter uma data em uma string usando o objeto `DateFormatter` e o método `string(from:)`. Veja um exemplo:

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let stringDate = dateFormatter.string(from: date)

print(stringDate)
// Output: 15/12/2021
```

Você pode personalizar o formato da data alterando o `dateFormat` de acordo com suas necessidades. Existem vários padrões de data disponíveis e você pode até criar o seu próprio formato.

## Mergulho Profundo

Além do `dateFormat`, o `DateFormatter` também oferece outras propriedades e métodos para personalizar ainda mais a forma como a data é apresentada em uma string. Por exemplo, você pode especificar um local, definir um estilo de data e hora específico e até mesmo adicionar símbolos ou textos junto com a data.

Outra opção é usar `NSDateComponentsFormatter` para formatar uma data em componentes, como ano, mês e dia. Isso pode ser útil quando se trabalha com intervalos de tempo ou quando se deseja uma representação mais precisa da data.

## Veja também

- [Documentação oficial da Apple sobre DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial sobre formatação de data em Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)
- [Vídeo explicando a conversão de data em string em Swift](https://www.youtube.com/watch?v=c00GaqgCYrU)