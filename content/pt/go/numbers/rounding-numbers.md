---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:40.210715-07:00
description: "Arredondar n\xFAmeros \xE9 ajustar o valor de um n\xFAmero para o inteiro\
  \ mais pr\xF3ximo ou para um n\xFAmero espec\xEDfico de casas decimais. Programadores\
  \ fazem isso por\u2026"
lastmod: '2024-03-13T22:44:46.054989-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros \xE9 ajustar o valor de um n\xFAmero para o inteiro\
  \ mais pr\xF3ximo ou para um n\xFAmero espec\xEDfico de casas decimais. Programadores\
  \ fazem isso por\u2026"
title: "Arredondando n\xFAmeros"
weight: 13
---

## O Que & Porquê?

Arredondar números é ajustar o valor de um número para o inteiro mais próximo ou para um número específico de casas decimais. Programadores fazem isso por razões como melhorar a legibilidade, simplificar cálculos ou atender a requisitos de precisão específicos do domínio.

## Como fazer:

Em Go, não há uma função integrada que arredonde números diretamente para um número específico de casas decimais no pacote math. No entanto, você pode alcançar o arredondamento por meio de uma combinação de funções para números inteiros ou implementar uma função personalizada para casas decimais.

### Arredondando para o inteiro mais próximo:

Para arredondar para o inteiro mais próximo, você pode usar a função `math.Floor()` adicionada de 0.5 para números positivos, e `math.Ceil()` menos 0.5 para números negativos, dependendo da direção para a qual você quer arredondar.

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(math.Floor(3.75 + 0.5))  // Saída: 4
	fmt.Println(math.Ceil(-3.75 - 0.5)) // Saída: -4
}
```

### Arredondando para um número específico de casas decimais:

Para arredondar para um número específico de casas decimais, pode-se usar uma função personalizada onde você multiplica o número por 10^n (onde n é o número de casas decimais), arredonda para o inteiro mais próximo como antes, e então divide por 10^n.

```go
package main

import (
	"fmt"
	"math"
)

func roundToDecimalPlace(number float64, places int) float64 {
	shift := math.Pow(10, float64(places))
	return math.Round(number*shift) / shift
}

func main() {
	fmt.Println(roundToDecimalPlace(3.14159, 2)) // Saída: 3.14
	fmt.Println(roundToDecimalPlace(-3.14159, 3)) // Saída: -3.142
}
```

## Aprofundamento

Arredondar números é uma operação fundamental na programação de computadores, ligada ao desafio histórico de representar números reais em um sistema binário. A necessidade de arredondamento surge porque muitos números reais não podem ser representados com precisão em binário, levando a erros de aproximação.

Em Go, a abordagem para o arredondamento é um tanto manual comparada com linguagens que oferecem funções integradas de arredondamento para casas decimais específicas. No entanto, o pacote `math` da biblioteca padrão do Go fornece os blocos de construção básicos (como `math.Floor` e `math.Ceil`) para construir qualquer mecanismo de arredondamento necessário pela aplicação.

Esta abordagem manual, embora aparentemente mais trabalhosa, oferece aos programadores um controle mais fino sobre como os números são arredondados, atendendo às necessidades de precisão e exatidão de diferentes aplicações. Alternativas como bibliotecas de terceiros ou projetar funções de arredondamento personalizadas podem fornecer soluções mais diretas quando se lida com números complexos ou requerendo operações matemáticas mais avançadas não cobertas pela biblioteca padrão.

Em conclusão, embora a biblioteca padrão de Go possa não oferecer funcionalidade direta de arredondamento para casas decimais, seu conjunto abrangente de funções matemáticas permite que os desenvolvedores implementem soluções robustas de arredondamento adaptadas às suas necessidades específicas.
