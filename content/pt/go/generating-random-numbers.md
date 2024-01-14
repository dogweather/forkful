---
title:                "Go: Gerando números aleatórios"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Porque

Gerar números aleatórios é uma tarefa comum em programação e pode ser útil em diversas situações. Pode ser usado para criar senhas, sorteios, simulações, entre outros. No Go, existe uma biblioteca nativa que facilita esse processo, tornando-o ideal para quem precisa de números aleatórios com eficiência.

## Como fazer

Para gerar números aleatórios em Go, precisamos usar a biblioteca "math/rand". É necessário importá-la no início do código para que possamos acessar suas funcionalidades. Em seguida, podemos utilizar a função `rand.Intn()` para gerar um número inteiro aleatório entre 0 e um limite especificado (não incluído). Por exemplo, para gerar um número entre 1 e 100:

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    numero := rand.Intn(100) + 1
    fmt.Println(numero)
}
```

A saída desse código será um número aleatório entre 1 e 100, que pode variar a cada execução.

Além da função `Intn()`, também podemos utilizar outras funções da biblioteca "math/rand" para gerar números aleatórios de outros tipos, como `Float32()` e `Float64()` para números decimais e `Perm()` para gerar uma permutação aleatória de uma sequência de números.

## Mergulho Profundo

Embora a geração de números aleatórios pareça simples, é importante ter em mente que eles não são realmente "aleatórios", mas sim pseudoaleatórios. Ou seja, eles são gerados a partir de uma sequência determinística de cálculos matemáticos, mas para um observador externo, essa sequência parece aleatória.

Essa diferenciação é importante, já que pode influenciar em aplicações que precisam de uma verdadeira aleatoriedade, como criptografia. Para esses casos, é necessário utilizar um gerador de números aleatórios criptograficamente seguro (CSPRNG) em vez da biblioteca "math/rand" do Go.

## Veja também

- [Documentação oficial do pacote math/rand do Go](https://pkg.go.dev/math/rand)
- [Tutorial sobre geração de números aleatórios em Go](https://tutorialedge.net/golang/go-random-number-generation-tutorial/)
- [Comparação entre geradores de números aleatórios no Go](https://blog.benjojo.co.uk/post/golang-prngs-picking-the-best-prng-in-go)