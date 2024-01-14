---
title:    "Go: Convertendo uma data em uma string"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que utilizar conversão de data em string?

Converter uma data em string é uma tarefa frequente em programação, especialmente ao lidar com dados em sistemas e aplicativos. Isso permite que as datas sejam armazenadas, manipuladas e exibidas de forma mais eficiente e amigável para o usuário. Além disso, é especialmente útil quando se trabalha com APIs ou conectando sistemas que utilizam diferentes formatos de data.

## Como realizar a conversão em Go?

A linguagem de programação Go possui uma função incorporada chamada `Format` que permite converter uma data em um determinado formato de string. Para usar essa função, é preciso importar o pacote `"time"` e definir a data a ser convertida com o método `Now()`. Em seguida, basta especificar o formato desejado dentro do método `Format()` e o resultado será uma string contendo a data no formato especificado.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Now()
	stringDate := date.Format("02/01/2006") // formato "dd/mm/aaaa"
	fmt.Println(stringDate) // saída: 04/11/2021
}
```

É importante salientar que o formato de data utilizado na função `Format()` deve ser sempre baseado no dia, mês e ano, seguindo o padrão `dd/mm/aaaa`. Para obter outras informações como hora, minutos e segundos, é preciso especificar no formato a ser utilizado.

## Aprofundando na conversão de data em string

Além do método `Format()`, a linguagem Go também possui outras opções para converter uma data em string, como por exemplo, o método `Parse()` que permite converter uma string em data. Além disso, também é possível definir formatos customizados para a conversão.

Outro aspecto importante a ser lembrado é que, ao trabalhar com diferentes fusos horários, é preciso considerar isso na hora de converter a data. A linguagem Go possui o pacote `"time/zoneinfo"` que permite consultar informações sobre fusos horários e fazer a conversão corretamente.

## Veja também

- [Documentação oficial do pacote time em Go](https://golang.org/pkg/time/)
- [Tutorial sobre conversão de datas em strings em Go](https://golangbyexample.com/golang-convert-string-to-date-format/)
- [Exemplos de formatos de data em Go](https://programming.guide/go/format-parse-string-time-date-example.html)