---
title:    "Go: Gerando números aleatórios"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Go?

Gerar números aleatórios é uma tarefa comum em programação e pode ser útil em diversas aplicações, como jogos, sorteios, criptografia e testes de software. Em Go, existem várias maneiras de gerar números aleatórios, permitindo que os desenvolvedores escolham a abordagem mais adequada para suas necessidades.

## Como gerar números aleatórios em Go?

Existem duas principais formas de gerar números aleatórios em Go: usando a função `rand.Intn()` e implementando um gerador de números pseudoaleatórios personalizado.

Para usar a função `rand.Intn()`, primeiro importamos o pacote `rand` e definimos o limite máximo do intervalo de números para gerar. Em seguida, usamos a função `Intn()` para gerar um número aleatório entre 0 e o limite definido.

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // Definindo o limite máximo como 10
    limite := 10
    // Gerando um número aleatório entre 0 e 10
    numero := rand.Intn(limite)
    // Imprimindo o número gerado
    fmt.Println(numero)
}
```

O código acima irá gerar um número aleatório diferente a cada vez que for executado. Para um valor mais imprevisível, podemos definir a semente do gerador de números aleatórios usando a função `rand.Seed()`.

Para implementar um gerador de números pseudoaleatórios personalizado, podemos usar a biblioteca `crypto/rand` e a função `Reader()` para criar uma fonte de números aleatórios criptograficamente segura. Em seguida, usamos a função `Read()` para gerar um slice de bytes aleatórios e convertemos para o tipo de dado desejado.

```Go
package main

import (
    "fmt"
    "crypto/rand"
    "math/big"
)

func main() {
    // Criando uma fonte de números aleatórios
    fonte := rand.Reader
    // Gerando um slice de bytes aleatórios
    bytes := make([]byte, 8)
    _, err := fonte.Read(bytes)
    if err != nil {
        // Tratando possíveis erros
        fmt.Println("Erro ao gerar número aleatório:", err)
    } else {
        // Convertendo para o tipo de dado "big.Int"
        numero := big.Int(bytes)
        // Imprimindo o número gerado
        fmt.Println(numero)
    }
}
```

## Deep Dive: Entendendo a geração de números aleatórios em Go

A função `rand.Intn()` utiliza um gerador de números pseudoaleatórios baseado em um algoritmo linear congruencial. Isso significa que cada número gerado é determinado por um cálculo matemático a partir do número anterior, o que pode resultar em uma sequência de números previsível. Para evitar isso, a função `rand.Seed()` é utilizada para definir uma semente aleatória inicial para o gerador.

Já a implementação customizada utilizando a biblioteca `crypto/rand` é mais segura e imprevisível, pois utiliza fontes criptográficas de aleatoriedade para gerar os números. No entanto, é um processo mais complexo e pode ser desnecessário em muitos casos de uso.

## Veja também

- Documentação oficial sobre a função `rand.Intn()`: https://golang.org/pkg/math/rand/#Intn
- Artigo sobre geradores de números pseudoaleatórios: https://blog.golang.org/realistic-random-numbers
- Perguntas frequentes sobre a biblioteca `crypto/rand`: https://golang.org/pkg/crypto/rand/#FAQ