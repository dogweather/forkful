---
title:                "Go: Convertendo uma data em uma string"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com datas em um programa em Go, é comum a necessidade de converter uma data em uma string. Isso pode ser útil, por exemplo, para exibir datas em um formato específico ou para enviar dados para um banco de dados que aceita apenas strings. Neste post, vamos explorar como realizar essa conversão em Go.

## Como fazer

Converter uma data em uma string em Go é bastante simples. A linguagem já possui uma função nativa para isso, chamada `Format()`, que permite especificar o formato da data desejada. Veja um exemplo de código:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Obtendo a data atual
    date := time.Now()

    // Convertendo a data em uma string com o formato "DD/MM/AAAA"
    dateString := date.Format("02/01/2006")

    // Imprimindo a string resultante
    fmt.Println(dateString)
}
```

Ao executar este código, o valor impresso será a data atual no formato "DD/MM/AAAA". Vale ressaltar que o formato utilizado na função `Format()` segue algumas convenções específicas. Por exemplo, "02" representa o dia, "01" o mês e "2006" o ano. É importante consultar a documentação oficial para ter uma lista completa de todos os formatos disponíveis.

## Deep Dive

Além da função `Format()`, existem outras formas de converter uma data em uma string em Go. Uma delas é utilizando a biblioteca `strconv`, que oferece a função `AppendTime()` para essa finalidade. Esta função possui três argumentos: um slice de bytes para armazenar o resultado, o formato da data e a data em si. Veja um exemplo de código:

```Go
package main

import (
    "fmt"
    "strconv"
    "time"
)

func main() {
    // Obtendo a data atual
    date := time.Now()

    // Criando um slice de bytes para armazenar o resultado
    var buffer []byte

    // Convertendo a data em uma string com o formato "YYYY-MM-DD"
    buffer = date.AppendFormat(buffer, "2006-01-02")

    // Imprimindo a string resultante
    fmt.Println(string(buffer))
}
```

A saída deste código será exatamente o mesmo que o exemplo anterior. No entanto, utilizando a função `AppendFormat()` é possível customizar ainda mais o resultado, como por exemplo, adicionar informações sobre o fuso horário ou o dia da semana.

## Veja também

- [Documentação oficial de strings em Go](https://golang.org/pkg/strconv/)
- [Documentação oficial de datas em Go](https://golang.org/pkg/time/)