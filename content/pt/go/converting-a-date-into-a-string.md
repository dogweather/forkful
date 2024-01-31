---
title:                "Convertendo uma data em uma string"
date:                  2024-01-20T17:36:32.134255-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Converter uma data em uma string é o processo de transformar um objeto de data (com valores numéricos para dia, mês, ano, etc.) numa cadeia de caracteres formatada. Fazemos isso para mostrar datas de maneira legível para humanos ou para prepará-las para serem armazenadas e transmitidas.

## Como Fazer:
```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Captura a data e hora atual
    now := time.Now()

    // Formata a data para string: yyyy-mm-dd
    fmt.Println(now.Format("2006-01-02"))

    // Formata a data para string no formato completo
    fmt.Println(now.Format("Monday, 02-Jan-06 15:04:05 MST"))
}

// Saída de exemplo
// 2023-03-28
// Tuesday, 28-Mar-23 21:52:31 PST
```

## Aprofundando:
Converter uma data em uma string é fundamental desde os primórdios da computação. Em Go, essa funcionalidade é proporcionada pelo pacote `time`, que oferece diversas maneiras de formatar uma data. A estrutura de formatação pode parecer estranha a princípio: Go utiliza uma data de referência específica (`Mon Jan 2 15:04:05 MST 2006`) para representar o padrão de formatação. Para formatos diferentes, existem bibliotecas alternativas como `github.com/jinzhu/now` ou até mesmo o uso de `strconv` para montar uma string manualmente, mas o padrão do Go é geralmente o mais simples e preferido. Implementar uma formatagem customizada envolve criar uma string de layout que utilize a data de referência, e o objeto de data que se quer formatar chamará o método `Format` com essa string de layout como argumento.

## Veja Também:
- Documentação oficial do Go para o pacote `time`: https://pkg.go.dev/time
- Go by Example com exemplos de formatação de datas: https://gobyexample.com/time-formatting-parsing
- Uma discussão sobre como escolher entre o pacote `time` e bibliotecas de terceiros: https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format
