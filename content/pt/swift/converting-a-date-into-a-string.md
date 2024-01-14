---
title:    "Swift: Convertendo uma data em string"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Se você já trabalhou com datas em Swift, sabe que em algum momento pode ser necessário converter uma data em uma string. Isso pode ser útil para exibir informações em um formato legível para o usuário, como em um aplicativo de calendário ou em um formulário de inscrição. Neste post, vamos explorar como fazer isso de maneira simples e eficaz.

## Como fazer:

```Swift
// Criando uma data
let data = Date()

// Criando um objeto DateFormatter
let formatter = DateFormatter()

// Definindo o formato da string
formatter.dateFormat = "dd/MM/yyyy"

// Convertendo a data em uma string
let dataString = formatter.string(from: data)
```

Output: "22/10/2021"

Acima, podemos ver que o processo de conversão é bastante simples. Primeiro, criamos uma data atual usando a classe Date. Em seguida, criamos um objeto DateFormatter e definimos o formato da string desejado. Por fim, usamos o método `string(from:)` para converter a data em uma string de acordo com o formato definido.

## Mergulho profundo:

Há algumas coisas importantes para se ter em mente ao converter datas em strings em Swift. Uma delas é o uso de diferentes formatos de data de acordo com a localização do usuário. Para isso, podemos usar a propriedade `Locale` para especificar a localização desejada.

Além disso, também é importante considerar a formatação de horas e fusos horários ao converter a data para uma string. Podemos utilizar métodos adicionais, como `setLocalizedDateFormatFromTemplate` e `setTimeZone`, para garantir que a string final seja exibida corretamente para o usuário.

## Veja também:

- [Documentação oficial do DateFormatter em Swift](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial sobre manipulação de datas em Swift](https://www.raywenderlich.com/7114164-swift-dates-getting-started)