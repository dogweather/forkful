---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios em programação é sobre criar valores que não podem ser previstos logicamente, essenciais para tarefas como testes de estresse e implementações de jogos. 

## Como Fazer:

Vamos começar criando um número aleatório entre 0 e 1.

```Go
package main
import (
  "fmt"
  "math/rand"
  "time"
)

func main() {
  rand.Seed(time.Now().UnixNano())
  fmt.Println(rand.Float64())
}
```
A saída será um número aleatório entre 0 e 1, como "0.4198234660039394".

Para criar um número inteiro aleatório dentro de um intervalo específico, como de 1 a 10:

```Go
package main
import (
  "fmt"
  "math/rand"
  "time"
)

func main() {
  rand.Seed(time.Now().UnixNano())
  fmt.Println(rand.Intn(10) + 1)
}
```
A saída será um número aleatório entre 1 e 10.

## Aprofundando:

Historicamente, a geração de números aleatórios em computadores tem sido um desafio, visto que eles naturalmente seguem uma sequência lógica. Go, derivado da linguagem de programação C, usa um gerador de números pseudoaleatórios, que produz uma sequência de números aparentemente sem relação lógica.

Uma alternativa é usar o pacote `crypto/rand` para uma geração verdadeiramente aleatória, uma abordagem mais segura, mas mais lenta. 

Em termos dos detalhes da implementação, Go baseia-se no tempo atual para semear (seed) o gerador de números aleatórios. Por isso, se correr o código rapidamente em sucessão, você pode receber o mesmo número, já que o tempo (em nanossegundos) pode não ter mudado.

## Veja Também:

1. Documentação oficial de Go para o pacote de random [math/rand](https://golang.org/pkg/math/rand/)
2. Introdução à geração de números aleatórios [Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
3. Documentação de Go para o pacote de random [crypto/rand](https://golang.org/pkg/crypto/rand/)