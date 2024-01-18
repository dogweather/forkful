---
title:                "Analisando uma data de uma string"
html_title:           "Go: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?
Parsear uma data de uma string em Go é simplesmente converter uma string que contém uma data em um formato legível para uma variável do tipo data. Os programadores fazem isso para poderem manipular e usar as datas em seus códigos de forma mais eficiente e precisa.

## Como fazer:
Exemplo de código Go com a função "Parse" para converter uma string em uma variável do tipo data:

```Go
dataString := "12-05-2021"
layout := "02-01-2006"
data, err := time.Parse(layout, dataString)

if err != nil {
    fmt.Println(err)
} else {
    fmt.Println(data)
}
```

A saída desse código seria: 2021-12-05 00:00:00 +0000 UTC.

## Mergulho Profundo:
Parsear datas de strings é uma técnica comum utilizada em muitas linguagens de programação. No passado, era comum os programadores terem que escrever códigos extensos e complexos para realizar essa conversão. Porém, em Go, temos a função "Parse" que torna esse processo mais simples e eficiente.

Uma alternativa para a função "Parse" é a função "ParseInLocation" que permite definir manualmente a localização e fuso horário da data a ser convertida.

No nível da implementação, a função "Parse" utiliza a função "NewScanner" para percorrer a string e encontrar os elementos correspondentes a dia, mês e ano. Em seguida, usa a função "parseNumber" para converter esses elementos em valores numéricos e finalmente retorna a data em formato de variável.

## Veja Também:
Para mais informações sobre a função "Parse" e outras funcionalidades de manipulação de datas em Go, consulte a documentação oficial em: https://golang.org/pkg/time/

Outra fonte útil é o tutorial sobre parsing de datas em Go do site A Tour of Go em: https://tour.golang.org/basics/15