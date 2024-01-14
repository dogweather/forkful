---
title:    "Go: Gerando números aleatórios"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante em Go

Gerar números aleatórios é uma tarefa essencial em muitos programas e projetos de programação. Em Go, essa função pode ser especialmente útil para aprimorar a segurança de um sistema, criar jogos interativos ou simulações realistas. Além disso, trabalhar com números aleatórios pode ser uma maneira divertida de aprender novos conceitos de programação e explorar a criatividade.

## Como gerar números aleatórios em Go

Para gerar números aleatórios em Go, primeiro é necessário importar o pacote "math/rand". Em seguida, podemos utilizar o método "Intn()" que gera um número inteiro aleatório no intervalo especificado. Por exemplo:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	numero := rand.Intn(100) // Gera um número aleatório entre 0 e 100
	fmt.Println(numero)
}
```

A saída deste código pode variar a cada vez que é executado, pois cada vez é gerado um novo número aleatório. Além disso, é possível utilizar o método "Float64()" para gerar um número decimal aleatório. Por exemplo:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	numero := rand.Float64() // Gera um número decimal aleatório entre 0 e 1
	fmt.Println(numero)
}
```

## Mergulho Profundo: Mais sobre geração de números aleatórios em Go

Em Go, a geração de números aleatórios é baseada no uso de um gerador de números pseudoaleatórios (PRNG). Isso significa que os números gerados não são tecnicamente "aleatórios", mas seguem um padrão matemático que, para a maioria das aplicações, é suficientemente aleatório. No entanto, é importante ressaltar que, dependendo do uso, pode ser mais apropriado utilizar um gerador de verdadeiros números aleatórios, como o "crypto/rand", que é mais seguro.

Além disso, em Go, é possível definir uma semente para o gerador de números aleatórios, o que pode ser útil para gerar a mesma sequência de números aleatórios em diferentes execuções do programa. Para fazer isso, podemos utilizar o método "Seed()" do pacote "rand", passando um número inteiro como argumento. Por exemplo:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	rand.Seed(42) // Define a semente para 42
	fmt.Println(rand.Intn(100)) // Gera um número aleatório entre 0 e 100
	fmt.Println(rand.Intn(100))
	fmt.Println(rand.Intn(100))
}
```

A saída deste código será sempre a mesma, gerando a sequência de números 74, 78 e 4.

## Veja também

- [Documentação oficial do pacote math/rand](https://golang.org/pkg/math/rand/)
- [Tutorial "Generating Random Numbers in Go" do Golangbot](https://golangbot.com/random-numbers/)
- [Artigo "Pseudo-Random Number Generation in Go" da DigitalOcean](https://www.digitalocean.com/community/tutorials/pseudo-random-number-generation-in-go)