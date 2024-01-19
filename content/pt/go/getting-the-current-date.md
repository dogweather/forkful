---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Oba, Codificadores Go! Aqui está Como Buscar a Data Atual em Go!

## O Que & Porquê?

Obter a data atual em um programa permite a rastreabilidade de eventos ou cálculos com base no tempo. É essencial para funções como marcação de tempo, registro de atividades e programação de eventos.

## Como fazer:

Aqui está como você pode obter a data atual em Go:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println("A data atual é:", currentDate)
}
```

Ao executar o código acima, você receberá uma saída como:

```Bash
A data atual é: 2022-03-16 01:12:34.5971068 +0000 UTC m=+0.000328600
```

## Deep Dive

Historicamente, a função `time.Now()`está desde as versões iniciais do Go. Seu uso é abrangente e intuitivo, tornando-o uma parte essencial do pacote "time". 

Existe uma alternativa, `time.Now().UTC()`, comumente utilizada quando não se deseja a hora local, mas a Universal Coordinated Time (UTC).

Há um pormenor interessante na implementação `time.Now()`: essa função retorna dois valores: a hora atual e a monotonic clock reading. A leitura do relógio monótono é o número de nanossegundos decorridos desde um tempo indefinido no passado. É útil para medir tempo decorrido diretamente, sem levar em consideração ajustes do relógio do sistema.

## Veja Também

Gostaria de se aprofundar mais? Confira esses recursos: 

1. Documentação Oficial Go sobre o Pacote `time` [aqui](https://golang.org/pkg/time/).
2. Um tutorial Go completo para iniciantes [aqui](https://www.tutorialspoint.com/go/index.htm).
3. Por que usamos UTC? Descubra [aqui](https://www.timeanddate.com/time/aboututc.html).