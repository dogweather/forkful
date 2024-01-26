---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:33.194752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Gerar números aleatórios é como rolar um dado virtual: você obtém resultados diferentes a cada vez. Programadores usam isso para tudo, desde jogos até segurança da informação.

## Como Fazer:
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Inicializa o gerador de números aleatórios
	rand.Seed(time.Now().UnixNano())

	// Gera um número aleatório entre 0 e 99
	numero := rand.Intn(100)
	fmt.Println("Número aleatório:", numero)

	// Gera um número aleatório em ponto flutuante entre 0.0 e 1.0
	float := rand.Float64()
	fmt.Println("Float aleatório:", float)
}
```
**Saída de Exemplo:**
```
Número aleatório: 42
Float aleatório: 0.7306181532387667
```

## Aprofundando:
Historicamente, gerar números realmente aleatórios é um desafio. Os computadores são ótimos em seguir instruções, mas péssimos em ser imprevisíveis. Por isso, normalmente usamos números pseudoaleatórios, que parecem aleatórios para quem está de fora, mas são gerados por um processo bem definido.

Em Go, a fonte padrão de aleatoriedade é determinística. Isso significa que, se você não alterar a "semente" com `rand.Seed`, você receberá a mesma sequência de números toda vez que rodar seu programa. É por isso que usamos `time.Now().UnixNano()` como semente, pois fornece um número único cada vez.

Existem alternativas como o pacote `crypto/rand`, que fornece números aleatórios mais seguros para uso em criptografia, onde a previsibilidade é uma ameaça séria à segurança.

## Veja Também:
- Documentação Oficial da Go sobre números aleatórios: [math/rand](https://pkg.go.dev/math/rand)
- Um post sobre segurança e aleatoriedade: [crypto/rand](https://pkg.go.dev/crypto/rand)
- Uma discussão no Stack Overflow sobre aleatoriedade em Go: [Stack Overflow: How to generate a random number?](https://stackoverflow.com/questions/12321133/how-to-properly-seed-random-number-generator)
