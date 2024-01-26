---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:31:03.077849-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Calcular uma data no futuro ou passado é basicamente ajustar um momento específico adicionando ou subtraindo durações temporais. Programadores fazem isso para agendar eventos, gerar lembretes, calcular prazos ou validar períodos de tempo para fins de consistência lógica e cronológica.

## Como Fazer:
Para adicionar ou subtrair dias de uma data, o pacote `time` em Go é seu melhor amigo. Aqui estão alguns exemplos de como fazer isso:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Pega a data e hora atual
	now := time.Now()
	fmt.Println("Data e Hora Atual:", now)

	// Adiciona 2 dias da data atual
	twoDaysLater := now.AddDate(0, 0, 2)
	fmt.Println("Daqui a Dois Dias:", twoDaysLater)

	// Subtrai 3 dias da data atual
	threeDaysBefore := now.AddDate(0, 0, -3)
	fmt.Println("Três Dias Atrás:", threeDaysBefore)
}
```
Saída de exemplo poderia ser:
```
Data e Hora Atual: 2023-03-15 10:23:42.651387 +0000 UTC m=+0.000123456
Daqui a Dois Dias: 2023-03-17 10:23:42.651387 +0000 UTC m=+0.000123456
Três Dias Atrás: 2023-03-12 10:23:42.651387 +0000 UTC m=+0.000123456
```

## Aprofundando:
Antigamente, calcular datas no futuro ou passado era feito manualmente ou com ferramentas simplistas. Hoje, bibliotecas de linguagens como Go tratam casos complexos, como anos bissextos ou diferentes fusos horários.

Algumas alternativas ao pacote `time` incluem bibliotecas de terceiros, como `dateparse` ou `go-carbon`, que oferecem funcionalidades adicionais.

Detalhes de implementação como lidar com fuso horário, formatar datas e horários, ou trabalhar com durações também são essenciais. No Go, a variável `Location` em conjunto com métodos como `time.ParseInLocation` e `time.Format`, te ajuda a lidar com esses detalhes com precisão.

## Veja Também:
- Documentação oficial do Go para o pacote `time`: https://golang.org/pkg/time/
- Go by Example, com exemplos práticos de uso de datas e horas: https://gobyexample.com/
- Go Time Package Tutorial: https://www.calhoun.io/working-with-dates-and-times-in-go/
- Uma comparação de bibliotecas de data em Go: https://medium.com/@nate510/don-t-use-go-s-default-time-package-5197bf4f8be2
