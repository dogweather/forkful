---
title:                "Gerando números aleatórios"
html_title:           "Go: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Gerar números aleatórios é um processo comum na programação, onde valores são criados de forma imprevisível, seguindo uma distribuição uniforme. Os programadores usam números aleatórios para diversas tarefas, como simular situações, criptografar dados ou tomar decisões aleatórias.

# Como fazer:

Para gerar números aleatórios em Go, podemos utilizar o pacote "math/rand". Primeiro, é necessário importá-lo em nosso código:

```Go
import "math/rand"
```

Em seguida, podemos usar a função "rand.Intn" para gerar um número inteiro aleatório entre 0 e o número especificado (exclusivo):

```Go
fmt.Println(rand.Intn(100)) // imprime um número aleatório entre 0 e 99
```

Também é possível gerar um número flutuante entre 0 e 1 usando a função "rand.Float64":

```Go
fmt.Println(rand.Float64()) // imprime um número flutuante aleatório entre 0 e 1
```

# Mergulho profundo:

A geração de números aleatórios é uma técnica amplamente utilizada em programação desde os primórdios da computação. Antes da criação dos computadores eletrônicos, já existiam métodos manuais para gerar números aleatórios, como jogar dados ou escolher cartas de baralho. Porém, com o avanço da tecnologia, surgiram diversas técnicas e algoritmos para a geração de números aleatórios em computadores, como o "Método do Meio Quadrado", o "Gerador Congruente Linear" e o "Gerador Mersenne Twister".

Além do pacote "math/rand", existem outras bibliotecas em Go que podem ser utilizadas para a geração de números aleatórios, como o "crypto/rand", que é mais adequado para tarefas que requerem um alto nível de segurança e criptografia.

A geração de números aleatórios também é uma área de estudo dentro da matemática e da estatística, com diversas aplicações em jogos, pesquisas e simulações.

# Veja também:

- [Documentação oficial do pacote "math/rand"](https://golang.org/pkg/math/rand/)
- [Outras bibliotecas relacionadas em Go](https://github.com/golang/go/wiki/Projects#random)
- [Artigo sobre a geração de números aleatórios](https://en.wikipedia.org/wiki/Random_number_generation) (em inglês)