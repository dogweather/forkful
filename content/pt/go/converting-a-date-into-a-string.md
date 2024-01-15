---
title:                "Convertendo uma data em uma string"
html_title:           "Go: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que
Se você está trabalhando com datas em um programa Go, em algum momento pode precisar convertê-las em strings. Isso é útil para exibir a data formatada em tela ou para escrevê-la em um arquivo de texto.

## Como fazer
O Go possui um pacote built-in chamado "time" que contém uma função chamada "Format" para converter datas em strings. Aqui está um exemplo simples:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    data := time.Now() // obtém a data atual
    dataString := data.Format("02/01/2006") // converte para o formato dd/mm/yyyy
    fmt.Println(dataString) // imprime a data formatada
}
```

Isso irá imprimir a data atual em formato dd/mm/yyyy, como por exemplo "24/06/2021". Você pode alterar o layout da string de acordo com suas necessidades, como adicionar o dia da semana ou mostrar a hora também. Aqui estão alguns layouts de string de data comuns:

- "02/01/2006" - dia/mês/ano
- "02-01-2006" - dia-mês-ano
- "02 de janeiro de 2006" - dia por extenso, mês por extenso, ano
- "02:15PM" - hora com a.m. ou p.m.

Existem muitas outras opções de layout e você pode conferir a documentação do pacote time para mais informações.

## Mergulho Profundo
Por baixo dos panos, a função "Format" do pacote time utiliza a funcionalidade de formatação de strings do pacote "fmt". Você pode até mesmo utilizar os verbos de formatação do "fmt" para personalizar ainda mais suas strings de data, como mostrado nesse exemplo:

```Go
dataString := fmt.Sprintf("%02d de %s de %d", data.Day(), data.Month(), data.Year())
```

Isso irá mostrar a data em formato "dd de mês por extenso de ano", como por exemplo "24 de junho de 2021".

## Veja Também
- Documentação do pacote time: https://golang.org/pkg/time/
- Documentação do pacote fmt: https://golang.org/pkg/fmt/#hdr-Printing
- Tutorial em vídeo sobre conversão de datas em strings: https://www.youtube.com/watch?v=XT0aWEMjq-s