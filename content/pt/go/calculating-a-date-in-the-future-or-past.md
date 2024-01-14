---
title:                "Go: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas futuras ou passadas pode ser uma tarefa comum para muitos programas, seja para agendar eventos, criar lembretes ou lidar com planejamento de tempo. No entanto, o Go tem uma biblioteca de gerenciamento de tempo robusta que pode facilitar essa tarefa de forma eficiente e eficaz.

## Como fazer

Para calcular datas futuras ou passdas com Go, podemos usar a função `AddDate()` da biblioteca `time`. Vamos dar uma olhada em um exemplo simples:

```Go
import (
    "fmt"
    "time"
)

func main() {
    agora := time.Now()
    dataFutura := agora.AddDate(0, 0, 7)

    fmt.Println("A data de hoje é:", agora.Format("02/01/2006"))
    fmt.Println("A data daqui a uma semana é:", dataFutura.Format("02/01/2006"))
}
```

Neste exemplo, usamos `AddDate()` para calcular a data de hoje mais 7 dias no futuro. Podemos alterar os valores passados como parâmetros para obter uma data mais distante no futuro ou no passado.

Podemos também calcular datas com mais precisão, especificando a quantidade de anos, meses e dias em vez de apenas dias. Por exemplo, `dataFutura := agora.AddDate(2, 3, 20)` irá adicionar 2 anos, 3 meses e 20 dias à data atual.

## Profundidade

Além da função `AddDate()`, a biblioteca `time` também possui outras funções úteis para lidar com datas, como `Date()`, `Parse()`, `Unix()` e muitas outras. Essas funções podem ajudar a formatar, converter e realizar cálculos com datas de forma mais precisa.

É importante notar que o Go usa `time.Location` para gerenciar diferenças de fuso horário, portanto, certifique-se de estar ciente disso ao realizar cálculos de datas em diferentes fusos horários.

## Veja também

- [Documentação da biblioteca time do Go](https://pkg.go.dev/time)
- [Tutorial de cálculo de datas em Go](https://gobyexample.com/time)
- [Exemplos avançados de cálculo de datas em Go](https://www.golangprograms.com/go-language/date-time.html)