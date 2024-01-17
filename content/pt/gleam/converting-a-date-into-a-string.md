---
title:                "Transformando uma data em uma string"
html_title:           "Gleam: Transformando uma data em uma string"
simple_title:         "Transformando uma data em uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

 Converter uma data em uma string é o processo de transformar uma data, que é uma forma de representar um momento específico no tempo, em uma cadeia de caracteres, que é uma coleção de letras e símbolos. Os programadores fazem isso para facilitar a manipulação e exibição de datas em seus programas.

## Como fazer:

```Gleam
import Gleam.Date
import Gleam.String

let date = Date.new(2021, 10, 15)
let string = String.from_date(date)
```
Saída: "2021-10-15"

```Gleam
import Gleam.Date
import Gleam.String

let date = Date.new(2021, 10, 15)
let string = String.from_date_with_format(date, "%d/%m/%Y")
```
Saída: "15/10/2021"

## Mergulho Profundo:

Existem várias maneiras de converter uma data em uma string, dependendo das necessidades do programador. Antes da linguagem de programação Gleam, métodos comuns incluíam o uso de bibliotecas externas ou escrever funções personalizadas. No entanto, com a biblioteca padrão `Gleam.Date` e a função `from_date_with_format`, essa tarefa foi simplificada.

Além disso, é importante observar que ao converter uma data em uma string, é necessário especificar o formato desejado. No exemplo acima, usamos "%d/%m/%Y" como formato, que significa dia/mês/ano. Isso permite que o programador escolha como a data será exibida, de acordo com suas preferências ou necessidades do programa.

## Veja também:

- Documentação da biblioteca `Gleam.Date`: https://gleam.run/std/datetime.html
- Tutorial sobre como trabalhar com datas em programas Gleam: https://gleam.run/book/times.html