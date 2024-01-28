---
title:                "Arredondamento de números"
date:                  2024-01-26T03:45:52.901928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Arredondar números significa ajustar um número para o inteiro mais próximo ou para o decimal especificado. Isso é feito para simplificar valores, torná-los mais legíveis, ou adequá-los a certas restrições, como ao trabalhar com moedas.

## Como fazer:
O pacote `math` do Go é seu amigo para arredondamentos. Use `math.Round`, `math.Floor` e `math.Ceil` para simplificar:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Arredonda para o inteiro mais próximo
	fmt.Println("Floor:", math.Floor(number)) // Arredonda para baixo
	fmt.Println("Ceil: ", math.Ceil(number))  // Arredonda para cima
}
```

Saída de exemplo:
```
Round: 3
Floor: 3
Ceil: 4
```

Para casas decimais específicas, multiplique, arredonde, depois divida:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Arredondado para 2 casas decimais:", roundToDecimalPlace(number, 2))
}
```

Saída de exemplo:
```
Arredondado para 2 casas decimais: 3.14
```

## Aprofundando
Arredondar números não é novidade—data de antigas matemáticas, sempre visando a simplicidade. O `math.Round` em Go utiliza o [arredondamento dos banqueiros](https://pt.wikipedia.org/wiki/Arredondamento#Arredondar_para_o_mais_próximo), significando que 0.5 arredonda para o número par mais próximo, reduzindo um viés que poderia afetar somas.

Números de ponto flutuante podem ser complicados devido à sua representação binária, que pode não representar exatamente todos os decimais. No entanto, a abordagem do Go mantém o comportamento esperado na maioria das vezes.

Existem outros métodos de arredondamento, como "arredondar para cima a metade" ou "arredondar a metade para longe de zero", mas a biblioteca padrão do Go é o que está prontamente disponível. Para necessidades mais complexas, você pode precisar de uma biblioteca de terceiros ou criar sua própria solução.

## Veja Também
- Pacote `math` do Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Padrão IEEE 754 para aritmética de ponto flutuante (base do Go para manipulação de floats): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Entendendo ponto flutuante: ["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
