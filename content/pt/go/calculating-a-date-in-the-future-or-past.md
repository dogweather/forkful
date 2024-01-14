---
title:    "Go: Calculando uma data no futuro ou passado"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que

Calcular uma data no passado ou no futuro é uma habilidade importante em programação. Pode ser usada para criar agendas, agendamentos de eventos, lembretes e muito mais. Aprender a calcular datas em Go pode tornar seu código mais dinâmico e funcional.

## Como Fazer

Existem algumas maneiras de calcular uma data no futuro ou no passado em Go. A maneira mais simples é usar a função `AddDate()` do pacote "time". Veja um exemplo abaixo:

```Go
import "fmt"
import "time"

func main() {
    data := time.Now()  // Obtém a data atual
    novoData := data.AddDate(1, 2, 3)  // Adiciona 1 ano, 2 meses e 3 dias à data atual
    fmt.Println(novoData)  // Imprime a nova data calculada
}
```

A saída desse código será "novoData: 2023-03-06 14:37:08.120899 +0000 UTC m=+0.000160354", representando a data atual adicionada com 1 ano, 2 meses e 3 dias.

Você também pode usar a função `Sub()` para subtrair uma quantidade de tempo de uma data específica. Veja um exemplo:

```Go
import "fmt"
import "time"

func main() {
    data := time.Now()  // Obtém a data atual
    dataAntiga := data.Sub(time.Hour * 24 * 7)  // Subtrai uma semana da data atual
    fmt.Println(dataAntiga)  // Imprime a data calculada
}
```

A saída deste código será "dataAntiga: 2022-07-16 14:37:08.120899 +0000 UTC m=+0.000160354", representando a data atual subtraída de uma semana.

## Mergulho Profundo

Para calcular uma data específica com mais precisão, é possível usar a estrutura `time.Time` e suas funções associadas, como `Date()` e `Seconds()`. Além disso, é possível trabalhar com fuso horários e criar datas personalizadas a partir de strings.

A função `Parse()` é extremamente útil nesses casos, pois permite que você converta uma string em um valor de data. Veja um exemplo:

```Go
import "fmt"
import "time"

func main() {
    dataString := "05/03/2025 18:30"  // Data e hora em formato de string
    formato := "02/01/2006 15:04"  // Formato da data e hora, de acordo com o layout da string
    data, _ := time.Parse(formato, dataString)  // Converte a string em um valor de data
    fmt.Println(data)  // Imprime a data convertida
}
```

A saída deste código será "2025-05-03 18:30:00 +0000 UTC", representando a data e hora fornecidas na string de acordo com o layout especificado.

## Veja Também

- [Pacote "time" em Go](https://golang.org/pkg/time/)
- [Documentação do pacote "time" em Português](https://github.com/GoBrasil/time)
- [Exemplos de cálculo de datas em Go](https://yourbasic.org/golang/add-time-date-millisecond-minute-hour/)