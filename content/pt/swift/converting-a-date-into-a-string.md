---
title:    "Swift: Convertendo uma data em uma sequência de caracteres"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Muitas vezes, é necessário converter uma data em uma string para exibi-la em um formato específico, como em um aplicativo de calendário ou em um sistema de reserva. O Swift oferece uma maneira fácil e eficiente de fazer essa conversão.

## Como fazer:

Usando o método `DateFormatter`, podemos especificar o formato desejado da data e, em seguida, convertê-la em uma string. Veja um exemplo simples abaixo:

```Swift
let data = Date()

let formato = DateFormatter()
formato.dateFormat = "dd/MM/yyyy"

let dataString = formato.string(from: data)

print(dataString) // output: 21/04/2021
```

Neste exemplo, criamos uma data atual usando o `Date()` e, em seguida, criamos um `DateFormatter` com o formato desejado. Por fim, usamos o `string(from:)` para converter a data em uma string.

Podemos personalizar ainda mais a conversão, adicionando a hora, segundos ou até mesmo as letras do dia da semana. Existem muitas opções disponíveis e você pode conferir a documentação do Swift para ver todas elas.

## Passo a passo:

1. Crie uma data usando `Date()`.
2. Crie um `DateFormatter` e defina o formato desejado.
3. Use o método `string(from:)` para converter a data em uma string.

## Mergulho profundo:

Além do método `string(from:)`, o Swift também possui o método `string(from:date:)` que nos permite especificar a data que queremos converter, em vez de usar a data atual. Além disso, podemos converter uma string em uma data usando o método `date(from:)`.

Outra opção interessante é usar diferentes estilos de data, como médio, curto ou longo, que já possuem um formato predefinido de acordo com a região do dispositivo.

## Veja também:

- Documentação do Swift sobre datas e formatação: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID348
- Tutorial sobre conversão de datas em strings em Swift: https://www.ralfebert.de/ios-examples/foundation/dateformatter/