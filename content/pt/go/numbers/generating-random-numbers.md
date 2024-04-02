---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:21.362678-07:00
description: "Gerar n\xFAmeros aleat\xF3rios na programa\xE7\xE3o trata de criar uma\
  \ sequ\xEAncia de n\xFAmeros que n\xE3o possa ser prevista de maneira razo\xE1vel\
  \ melhor do que por acaso.\u2026"
lastmod: '2024-03-13T22:44:46.056049-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios na programa\xE7\xE3o trata de criar uma sequ\xEA\
  ncia de n\xFAmeros que n\xE3o possa ser prevista de maneira razo\xE1vel melhor do\
  \ que por acaso.\u2026"
title: "Gerando n\xFAmeros aleat\xF3rios"
weight: 12
---

## O Que & Por Quê?

Gerar números aleatórios na programação trata de criar uma sequência de números que não possa ser prevista de maneira razoável melhor do que por acaso. Programadores fazem isso por uma miríade de razões, incluindo simulações, jogos e aplicações de segurança, onde a imprevisibilidade é chave para a funcionalidade ou secretismo.

## Como fazer:

Em Go, números aleatórios são gerados usando o pacote `math/rand` para números pseudo-aleatórios ou `crypto/rand` para números pseudo-aleatórios criptograficamente seguros. Vamos explorar ambos.

### Usando `math/rand` para Números Pseudo-Aleatórios

Primeiro, importe o pacote `math/rand` e o pacote `time` para semear o gerador. Semear garante que você obtenha uma sequência diferente de números a cada execução.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Um número aleatório:", rand.Intn(100)) // Gera um número entre 0 e 99
}
```

Saída de exemplo: `Um número aleatório: 42`

### Usando `crypto/rand` para Números Pseudo-Aleatórios Criptograficamente Seguros

Para aplicações mais sensíveis à segurança, o pacote `crypto/rand` é adequado, pois gera números aleatórios que são difíceis de prever, tornando-os adequados para operações criptográficas.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Um número aleatório seguro:", n)
}
```

Saída de exemplo: `Um número aleatório seguro: 81`

## Aprofundando

A diferença principal entre os pacotes `math/rand` e `crypto/rand` em Go vem da sua fonte de entropia e dos casos de uso pretendidos. `math/rand` gera números pseudo-aleatórios baseados numa semente inicial; assim, a sequência é determinística e pode ser prevista se a semente for conhecida. Isso é adequado para cenários onde o desempenho alto e não a imprevisibilidade absoluta é a principal preocupação, como em simulações ou jogos.

Por outro lado, `crypto/rand` deriva aleatoriedade do sistema operacional subjacente, tornando-o adequado para usos criptográficos onde a imprevisibilidade é crucial. No entanto, isso vem com o custo de desempenho e complexidade no manuseio dos números que ele gera (como lidar com o tipo `*big.Int` para inteiros).

Historicamente, a noção de geração de números aleatórios em computadores sempre dançou na borda da verdadeira "aleatoriedade", com sistemas antigos dependendo fortemente de algoritmos determinísticos que imitavam a aleatoriedade. Conforme os computadores evoluíram, esses algoritmos também evoluíram, incorporando fontes de entropia mais sofisticadas de seus ambientes.

Apesar desses avanços, a busca por uma aleatoriedade perfeita na computação é inerentemente paradoxal, dada a natureza determinística dos próprios computadores. É por isso que, para a maioria das aplicações onde a previsibilidade seria prejudicial, números pseudo-aleatórios criptograficamente seguros de fontes como `crypto/rand` são a alternativa melhor, apesar do seu overhead.

Em essência, a abordagem de Go com dois pacotes distintos para a geração de números aleatórios aborda elegantemente os compromissos entre desempenho e segurança, permitindo que os desenvolvedores escolham com base em suas necessidades específicas.
