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

## Porquê

Gerar números aleatórios é uma tarefa comum e fundamental em muitos programas. Pode ser necessário para testar algoritmos, criar chaves de criptografia ou selecionar itens aleatórios em um jogo. O Go possui uma biblioteca robusta para gerar números aleatórios, o que torna o processo mais fácil e eficiente.

## Como fazer

Para gerar números aleatórios em Go, primeiro é necessário importar a biblioteca "math/rand". Depois, é preciso definir uma fonte de aleatoriedade, por exemplo, usando a função "Seed()" para definir a semente de aleatoriedade ou "NewSource()" para criar uma nova fonte de números aleatórios.

Em seguida, podemos usar a função "Intn()" para gerar um número inteiro aleatório até um limite fornecido. Também é possível usar a função "Float64()" para gerar um número decimal aleatório.

Um exemplo de código que gera um número aleatório entre 1 e 10 seria:

```
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Definindo a semente de aleatoriedade com base no horário atual
	rand.Seed(time.Now().UnixNano())

	// Gerando um número aleatório entre 1 e 10
	numero := rand.Intn(10) + 1

	fmt.Printf("O número aleatório gerado é: %d", numero)
}
```

Output:

```
O número aleatório gerado é: 7
```

## Aprofundando-se

A biblioteca "math/rand" do Go é baseada em um algoritmo chamado "Mersenne Twister" e oferece uma ampla variedade de funções para gerar números aleatórios de diferentes tipos (inteiros, strings, etc) e limites.

É importante lembrar que, embora os números gerados pareçam aleatórios, eles são, na verdade, determinísticos com base na semente fornecida. Isso significa que duas execuções do mesmo programa com a mesma semente produzirão os mesmos resultados.

Além disso, a geração de números aleatórios não é uma tarefa totalmente segura para criptografia, já que algoritmos e seeds podem ser facilmente descobertos por pessoas mal-intencionadas.

## Veja também

- [Documentação oficial do pacote "math/rand" do Go](https://golang.org/pkg/math/rand/)
- [Tutorial sobre geração de números aleatórios em Go (em inglês)](https://www.callicoder.com/golang-random-number-generator/)
- [Artigo sobre segurança em algoritmos de geração de números aleatórios (em inglês)](https://www.schneier.com/academic/paperfiles/paper-prngs.pdf)