---
title:                "Geração de números aleatórios"
date:                  2024-01-27T20:33:46.914815-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios em Go envolve o uso do pacote `math/rand` para produzir números pseudoaleatórios para diversas aplicações, como simular experimentos, gerar dados de teste ou adicionar imprevisibilidade a jogos. Programadores utilizam esse recurso para criar comportamentos de software dinâmicos e menos previsíveis.

## Como Fazer:

Para começar a gerar números aleatórios em Go, você precisa importar o pacote `math/rand` e o pacote `time` para semear o gerador de números aleatórios para mais imprevisibilidade. Aqui está um exemplo básico:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Semear o gerador
	rand.Seed(time.Now().UnixNano())
	
	// Gerar um inteiro aleatório entre 0 e 99
	randomInt := rand.Intn(100)
	fmt.Println("Inteiro Aleatório:", randomInt)
	
	// Gerar um float aleatório entre 0.0 e 1.0
	randomFloat := rand.Float64()
	fmt.Println("Float Aleatório:", randomFloat)
}
```

A saída do exemplo poderia ser:

```
Inteiro Aleatório: 42
Float Aleatório: 0.7304601899194229
```

Lembre-se, cada execução produz números diferentes devido à semeadura com a hora atual.

## Aprofundando

O pacote `math/rand` em Go implementa geradores de números pseudoaleatórios (PRNGs) para várias distribuições. Embora bastante eficaz para muitas aplicações, é crucial notar que os números gerados pelo `math/rand` não são adequados para fins criptográficos devido à sua natureza determinística. Para necessidades criptográficas, o pacote `crypto/rand` é a escolha apropriada, fornecendo um gerador de números aleatórios seguro.

A implementação do `math/rand` é baseada em um algoritmo gerador de números aleatórios subtrativos, que é eficiente e tem um período relativamente longo antes de repetir sequências. No entanto, para aplicações que requerem sequências verdadeiramente aleatórias, como operações criptográficas, geradores de números aleatórios de hardware (RNGs) ou o pacote `crypto/rand`, que se comunica com fontes de aleatoriedade seguras específicas do sistema, são recomendados.

O `math/rand` permite a semeadura para introduzir variabilidade, mas a mesma semente sempre gerará a mesma sequência de números, destacando a natureza determinística de sua aleatoriedade. Isso o torna adequado para simulações ou jogos onde a reprodutibilidade pode ser desejável para fins de depuração ou testes.
