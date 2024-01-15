---
title:                "Obtendo a data atual."
html_title:           "Go: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Obter a data atual é uma tarefa comum em muitos aplicativos de software e pode ser útil para várias finalidades, como registro de eventos, agendamento de tarefas e geração de relatórios. Com o Go, é fácil obter a data atual e manipulá-la de acordo com as necessidades do seu projeto.

## Como fazer

Para obter a data atual em Go, podemos usar a função `time.Now()`. Ela retorna uma instância do tipo `Time`, que contém a data e hora atuais. Veja um exemplo de código abaixo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	agora := time.Now()
	fmt.Println("Data e hora atual:", agora)
}
```
```
Saída:
Data e hora atual: 2020-07-20 15:30:45.6789 +0000 UTC m=+0.000000000
```

Podemos manipular a data e hora obtida utilizando os métodos da struct `Time`, como `Year()`, `Month()`, `Day()` e `Hour()`. Veja um exemplo mostrando apenas a data atual no formato dia/mês/ano:

```Go
agora := time.Now()
fmt.Printf("Data atual: %d/%d/%d", agora.Day(), agora.Month(), agora.Year())
```
```
Saída:
Data atual: 20/7/2020
```

Também é possível adicionar ou subtrair um intervalo de tempo à data atual utilizando os métodos `Add()` e `Sub()`, respectivamente. Veja um exemplo de adição de um dia à data atual:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	agora := time.Now()
	amanha := agora.Add(time.Hour * 24)
	fmt.Println("Data atual:", agora)
	fmt.Println("Data de amanhã:", amanha)
}
```
```
Saída:
Data atual: 2020-07-20 15:30:45.6789 +0000 UTC m=+0.000000000
Data de amanhã: 2020-07-21 15:30:45.6789 +0000 UTC m=+86400.000000000
```

## Aprofundamento

A struct `Time` também possui métodos para formatar a data em diferentes layouts, além de permitir a comparação entre datas e a verificação de intervalo de tempo. Para obter mais informações sobre esses métodos e outras funcionalidades relacionadas à manipulação de datas e horas no Go, consulte a documentação oficial: https://golang.org/pkg/time/

## Veja também

- https://golang.org/pkg/time/
- https://www.calhoun.io/working-with-date-and-time-in-go/
- https://www.sohamkamani.com/blog/golang/working-with-time-in-go/