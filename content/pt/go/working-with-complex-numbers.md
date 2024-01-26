---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:40:58.957650-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Números complexos, compostos por uma parte real e uma imaginária (como 5 + 7i), são cruciais em campos como engenharia, física e processamento de sinais. Programadores trabalham com eles para resolver problemas nestes domínios que seriam difíceis de resolver apenas com números reais.

## Como Fazer:
Go tem suporte embutido para números complexos. Aqui está um rápido tutorial:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Criando números complexos
	a := complex(2, 3)
	b := 4 + 5i

	// Operações básicas
	fmt.Println("Adição:", a+b)
	fmt.Println("Subtração:", a-b)
	fmt.Println("Multiplicação:", a*b)
	fmt.Println("Divisão:", a/b)

	// Propriedades do número complexo
	fmt.Println("Parte real:", real(b))
	fmt.Println("Parte imaginária:", imag(b))
	fmt.Println("Conjugado:", cmplx.Conj(b))
	fmt.Println("Magnitude:", cmplx.Abs(b))
	fmt.Println("Ângulo de fase (radianos):", cmplx.Phase(b))
}

```

Saída de exemplo:

```
Adição: (6+8i)
Subtração: (-2-2i)
Multiplicação: (-7+22i)
Divisão: (0.5609756097560976+0.0487804878048781i)
Parte real: 4
Parte imaginária: 5
Conjugado: (4-5i)
Magnitude: 6.4031242374328485
Ângulo de fase (radianos): 0.8960553845713439
```

## Aprofundamento
Há tempos atrás, os números complexos eram vistos com suspeição — alguns pensavam que eram inúteis! Com o tempo, seu poder em descrever fenômenos físicos ficou claro. Eles são fundamentais em física quântica, teoria de controle e engenharia elétrica, só para citar algumas áreas.

No Go, os números complexos são representados usando um tipo de dado chamado `complex128` (64 bits para a parte real e imaginária cada) ou `complex64` (32 bits cada). Por baixo dos panos, são realmente apenas dois `float64`s ou `float32`s juntos. A biblioteca padrão do Go, `math/cmplx`, oferece funções para operações matemáticas complexas. Isso te salva da matemática complicada e permite que você se concentre em resolver problemas.

Alternativas ao suporte embutido de Go incluem o uso de bibliotecas externas ou criar seu próprio tratamento de números complexos. Mas isso raramente é necessário porque o suporte nativo do Go é eficiente e bem integrado à linguagem.

## Veja Também
Confira estes links para mais sobre as capacidades de número complexo do Go:
- Documentação oficial do Go: https://golang.org/pkg/math/cmplx/
- Um aprofundamento matemático sobre números complexos: https://www.mathsisfun.com/numbers/complex-numbers.html
- Aplicações práticas de números complexos em engenharia: https://ieeexplore.ieee.org/document/528dunno