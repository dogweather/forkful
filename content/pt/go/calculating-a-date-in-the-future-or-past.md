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

# Cálculos de data em Go: Um Guia Prático

## O Que & Porquê?

Calcular uma data futura ou passada é simplesmente identificar uma data que seja x dias, semanas, meses ou anos a partir de uma data base. Isso é útil para agendamentos de tarefas, verificação de prazos e diversos outros casos de uso.

## Como Fazer:

Usando o módulo "time" do Go, podemos facilmente calcular datas futuras e passadas. Veja o exemplo abaixo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dataBase := time.Now()
	diasParaAFrente := 30
	dataFutura := dataBase.AddDate(0, 0, diasParaAFrente)
	fmt.Println("Data Futura: ", dataFutura.Format("02-01-2006"))

	diasParaTras := -15
	dataPassada := dataBase.AddDate(0, 0, diasParaTras)
	fmt.Println("Data Passada: ", dataPassada.Format("02-01-2006"))
}
```
Neste código, estamos adicionando e subtraindo dias da data atual para conseguir uma data futura e uma data passada, respectivamente.

## Mergulho Profundo

O Go foi criado em 2007 no Google para resolver problemas de escalabilidade de sistemas grandes. Como parte de sua biblioteca padrão, Go tem uma série de conveniências para lidar com tempo e datas, como o método AddDate que estamos usando aqui. 

Existem várias outras maneiras de calcular a data futura ou passada em Go. Por exemplo, você pode usar a função `Add` ou `AddDate` do pacote `time`, dependendo se você quer expressar o período de tempo em horas, minutos, segundos ou como uma diferença de datas.

Detalhes de implementação é que `AddDate(years int, months int, days int) Time` ajusta a data, adicionando os períodos especificados de anos, meses e dias. Se a data resultante, na opinião da biblioteca de tempo, não existe, o mês, dia e até ano mais próximos são usados.

## Veja Também:

- Para mais informações e exemplos sobre o uso de data e tempo em Go, consulte a documentação oficial aqui: [https://golang.org/pkg/time](https://golang.org/pkg/time). 
- Para uma compreensão mais aprofundada do pacote "time" em Go, este portal abrange muitos detalhes: [https://gobyexample.com/time](https://gobyexample.com/time).
- Um guia mais amplo para iniciantes em Go pode ser encontrado neste link: [https://golangbot.com/learn-golang-series](https://golangbot.com/learn-golang-series).