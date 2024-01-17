---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Go: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Calcular uma data no futuro ou passado é uma tarefa comum para programadores que precisam lidar com datas e horários em seus códigos. Isso pode ser útil, por exemplo, para agendar tarefas em um sistema ou para criar funções de lembrete.

## Como fazer:
```Go
// Importar a biblioteca de data e hora
import "time"

// Definir uma data e hora de referência
refDate := time.Date(2020, time.March, 25, 12, 00, 00, 00, time.UTC)

// Calcular a data 3 dias no futuro
futureDate := refDate.AddDate(0, 0, 3)

// Calcular a data 2 meses no passado
pastDate := refDate.AddDate(0, -2, 0)

// Imprimir os resultados
fmt.Println(futureDate) // 2020-03-28 12:00:00 +0000 UTC
fmt.Println(pastDate) // 2020-01-25 12:00:00 +0000 UTC
```

## Mergulho Profundo:
Para calcular datas no futuro ou passado, a linguagem Go possui uma biblioteca de data e hora completa que permite manipular e formatar datas de diversas formas. Além disso, existem também bibliotecas externas, como a "dateparse", que oferecem funcionalidades extras, como o parsing de datas em diferentes formatos. Outras alternativas para calcular datas podem incluir a utilização de bibliotecas de terceiros, dependendo do uso em específico.

## Veja Também:
- [Time and date in Go](https://golang.org/pkg/time/)
- [Dateparse library](https://github.com/araddon/dateparse)