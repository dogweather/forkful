---
title:                "Go: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que utilizar números aleatórios em programas Go

Números aleatórios são uma ferramenta importante no desenvolvimento de programas em Go. Eles podem ser utilizados em diversas aplicações, desde jogos até geração de dados para testes. Além disso, a incorporação de números aleatórios em um programa pode torná-lo mais dinâmico e interessante para os usuários.

## Como gerar números aleatórios em Go

Em Go, podemos gerar números aleatórios utilizando a biblioteca `rand`. Primeiro, precisamos importar essa biblioteca no nosso programa:

```Go 
import "math/rand"
```

Em seguida, podemos utilizar a função `Intn()` para gerar um número inteiro aleatório no intervalo de 0 até o número passado como parâmetro. Por exemplo, para gerar um número aleatório entre 0 e 100, podemos utilizar o seguinte código:

```Go
numero := rand.Intn(100)
```

Também é possível gerar números aleatórios de outros tipos, como float, utilizando as funções `Float32()` e `Float64()` da biblioteca `rand`.

## Mergulhando mais fundo na geração de números aleatórios em Go

Por baixo dos panos, a função `rand.Intn()` utiliza um gerador pseudo-aleatório baseado em uma semente. Essa semente é definida pelo valor passado na função `rand.Seed()`. Se nenhum valor for passado, o gerador irá utilizar a hora atual do sistema como semente.

No entanto, para garantir resultados verdadeiramente aleatórios, é recomendado definir uma semente personalizada utilizando um valor randômico, como a hora atual em nanossegundos, por exemplo.

## Veja também

- [Documentação oficial da biblioteca `rand` em Go](https://golang.org/pkg/math/rand/)
- [Tutorial de geração de números aleatórios em Go](https://tutorialedge.net/golang/go-random-number-generation-tutorial/)
- [Vídeo tutorial sobre geração de números aleatórios em Go](https://www.youtube.com/watch?v=852Jr5XEjfk)