---
title:                "Swift: Convertendo uma data em uma sequência de caracteres"
simple_title:         "Convertendo uma data em uma sequência de caracteres"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de converter uma data em formato de string durante a programação em Swift? Isso pode ser útil em várias situações, como por exemplo, armazenar as datas em um banco de dados ou exibir informações na tela formatadas de acordo com a localização do usuário.

## Como fazer

Para converter uma data em string, podemos utilizar o `DateFormatter` em Swift. Este é um objeto que nos permite formatar as datas de acordo com as nossas necessidades. Vamos dar uma olhada em um exemplo:

```Swift
let today = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let dateString = dateFormatter.string(from: today)
print(dateString)
```

Neste código, primeiro criamos um objeto `Date` que representa a data atual. Em seguida, instanciamos um objeto `DateFormatter` e definimos seu formato de data para "dd-MM-yyyy", que significa dia-mês-ano. Finalmente, usamos o método `string(from:)` para converter a data em uma string e a imprimir na tela. O resultado será algo como "06-05-2021", dependendo da data atual.

Além do formato de data, podemos também definir o formato de hora, fuso horário, localização e muitas outras opções para personalizar a conversão de data em string. Você pode encontrar uma lista completa de formatos disponíveis [neste link](https://developer.apple.com/documentation/foundation/dateformatter/style).

## Mergulho aprofundado

Agora que já sabemos como converter uma data em string, é importante entender que tipo de dados essa string representa. Normalmente, quando convertemos uma data em uma string, estamos transformando-a em uma representação legível para humanos, mas não é necessariamente a forma mais útil para armazenamento ou cálculos.

Por exemplo, se tentarmos comparar duas datas convertidas em string, os resultados podem não ser os esperados, já que os caracteres são comparados em ordem alfabética e não em ordem cronológica. Além disso, é importante lembrar que diferentes localizações podem ter formatos de datas diferentes, o que pode causar problemas em aplicações multi-região.

Para evitar esses problemas, é importante sempre converter as datas em tipos de dados apropriados antes de fazer qualquer tipo de manipulação. Por exemplo, podemos converter a string de data de volta para o tipo `Date` usando o método `date(from:)` do nosso objeto `DateFormatter`. Este método nos permite especificar o formato esperado da string, garantindo a consistência dos dados.

## Veja também
- [Guia de referência da Apple sobre o DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Lista de formatos de data disponíveis](https://developer.apple.com/documentation/foundation/dateformatter/style)