---
title:                "Go: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em Go?

Em muitos projetos de programação, pode ser necessário obter a data atual para realizar cálculos ou exibir informações relevantes. No Go, isso pode ser feito de forma rápida e fácil, graças às funções embutidas da linguagem. Neste artigo, vamos explorar como obter a data atual em programas Go.

## Como fazer:

Para obter a data atual em Go, podemos usar a função ```time.Now()```, que retorna a data e hora atuais em um objeto ```time.Time```. Podemos então formatar essa data de acordo com nossas necessidades usando a função ```Format()```. Veja um exemplo abaixo:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    dataAtual := time.Now()
    dataFormatada := dataAtual.Format("02/01/2006")
    fmt.Println("A data atual é:", dataFormatada)
}
```

A saída deste código será: ```A data atual é: 08/06/2021```, já que a data está sendo formatada para exibir o dia, mês e ano no formato DD/MM/AAAA. Podemos alterar o layout da formatação para atender às nossas necessidades, seguindo as diretrizes encontradas na documentação da linguagem.

## Mergulho Profundo:

Para entender melhor como a função ```time.Now()``` funciona, é importante saber que ela retorna o horário de acordo com a localização do seu computador. Se você quiser obter a data e hora em um fuso horário específico, podemos usar a função ```time.LoadLocation()``` para especificar o local desejado. Veja um exemplo abaixo:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    local, _ := time.LoadLocation("America/Sao_Paulo")
    dataAtual := time.Now().In(local)
    fmt.Println("A data atual em São Paulo é:", dataAtual)
}
```

A saída deste código será: ```A data atual em São Paulo é: 2021-06-08 10:00:00 -0300 -03```, considerando que a data e hora atual da realização deste exemplo foi 08/06/2021 às 10:00.

## Veja também:

Para saber mais sobre como trabalhar com datas e horas em Go, confira a documentação oficial da linguagem: [https://golang.org/pkg/time/](https://golang.org/pkg/time/). Além disso, você pode aprender mais sobre outras funções e recursos do Go em: [https://blog.golang.org/](https://blog.golang.org/).