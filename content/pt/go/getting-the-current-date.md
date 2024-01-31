---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:14:56.876951-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Buscar a data atual é simplesmente obter a data do momento em que a execução do programa está acontecendo. Programadores fazem isso para logs, funções de data e hora e qualquer funcionalidade que necessite da noção de "agora".

## Como Fazer:
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current := time.Now()
    fmt.Println("Data e Hora Atuais:", current)
}
```
Output:
```
Data e Hora Atuais: 2023-04-02 15:04:05.999999999 -0300 -03
```

## Mergulho Profundo
A função `time.Now()` no Go retorna um valor do tipo `time.Time` que representa o instante atual com precisão de nanossegundos. Historically, as estruturas de data e hora evoluíram desde simples timestamps até objetos complexos. Em Go, a decisão foi criar um tipo robusto e conciso que facilita não só a obtenção da data atual mas também manipulação e formatação.

Existem alternativas, como usar uma biblioteca de terceiros, mas `time.Now()` é suficiente para a maioria dos casos e é mantida pela própria equipe do Go. Em termos de implementação, Go lida com tempo real obtendo informações diretamente do sistema operacional, o que significa que a precisão e exatidão dependem também do SO.

## Veja Também
- Documentação oficial de `time`: https://golang.org/pkg/time/
- Artigo sobre manipulação de tempo em Go: https://yourbasic.org/golang/time-change-date/
- Repositório Go (Exemplos de `time` package): https://github.com/golang/go/tree/master/src/time
