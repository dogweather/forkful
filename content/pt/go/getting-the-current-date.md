---
title:    "Go: Obtendo a data atual"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que obter a data atual pode ser útil

Há várias razões pelas quais alguém pode precisar obter a data atual em um programa Go. Uma delas pode ser para fins de registro ou para executar uma tarefa diária. Independentemente do motivo, saber como obter a data atual é uma habilidade importante para qualquer programador.

## Como obter a data atual em Go

Obter a data atual em Go é muito simples. Basta importar o pacote "time" e usar o método "Now()" no tipo de dados "time". Veja o exemplo abaixo:

```Go
pacote principal

import (
    "fmt"
    "time"
)

func main() {
    dataAtual := time.Now()
    fmt.Println(dataAtual)
}
```

A saída desse código será algo semelhante a "2021-09-13 12:00:00 +0000 UTC". Você também pode formatar a data de acordo com as suas necessidades usando o método "Format()". Por exemplo:

```Go
pacote principal

import (
    "fmt"
    "time"
)

func main() {
    dataAtual := time.Now()
    fmt.Println(dataAtual.Format("02/01/2006"))
}
```

Nesse exemplo, a saída será no formato "13/09/2021". Você pode experimentar diferentes layouts de formato para obter a data desejada.

## Mergulho profundo em obter a data atual em Go

O método "Now()" retorna a data atual no formato de objeto "time". Esse objeto possui outros métodos úteis para manipular a data, como "AddDate()" para adicionar anos, meses e dias. Além disso, você também pode usar o método "Unix()" para obter a data atual em segundos desde 1970.

## Veja também

- [Documentação oficial do pacote time](https://golang.org/pkg/time/)
- [Tutorial de formatação de datas em Go](https://www.sohamkamani.com/golang/formatting-time/)
- [Exemplos de uso do pacote time em Go Playground](https://play.golang.org/p/wf0b5yDSj7I)