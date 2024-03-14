---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:03.504025-07:00
description: "Trabalhar com n\xFAmeros complexos na programa\xE7\xE3o envolve manipular\
  \ n\xFAmeros que possuem tanto uma parte real quanto uma imagin\xE1ria, tipicamente\
  \ expressos\u2026"
lastmod: '2024-03-13T22:44:46.053907-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com n\xFAmeros complexos na programa\xE7\xE3o envolve manipular\
  \ n\xFAmeros que possuem tanto uma parte real quanto uma imagin\xE1ria, tipicamente\
  \ expressos\u2026"
title: "Trabalhando com N\xFAmeros Complexos"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com números complexos na programação envolve manipular números que possuem tanto uma parte real quanto uma imaginária, tipicamente expressos como `a + bi`. Programadores lidam com números complexos em vários domínios, como engenharia, física e análise de dados, para resolver problemas envolvendo raízes quadradas de números negativos, análises de forma de onda e mais.

## Como fazer:

Em Go, números complexos são manipulados usando as funções embutidas `complex`, `real` e `imag`, juntamente com os tipos `complex64` e `complex128` (representando números complexos de 64 bits e 128 bits, respectivamente). Aqui está um guia rápido para começar:

```go
package main

import (
	"fmt"
)

func main() {
	// Criando números complexos
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Operações aritméticas
	c := a + b
	fmt.Println("Adição:", c) // Saída: Adição: (3+2i)

	d := a * b
	fmt.Println("Multiplicação:", d) // Saída: Multiplicação: (5+1i)

	// Acessando partes real e imaginária
	parteReal := real(a)
	parteImaginaria := imag(a)
	fmt.Printf("Parte real: %.1f, Parte imaginária: %.1f\n", parteReal, parteImaginaria) // Saída: Parte real: 2.0, Parte imaginária: 3.0

	// Conjugado complexo e magnitude podem ser calculados
	conjugado := complex(real(a), -imag(a)) // Manualmente
	fmt.Println("Conjugado de a:", conjugado) // Saída: Conjugado de a: (2-3i)
}
```

Este exemplo cobre o básico, mas há muito mais que você pode fazer com números complexos, incluindo o aproveitamento do pacote `math/cmplx` para operações mais avançadas como encontrar a magnitude, fase e muito mais.

## Mergulho Profundo

O conceito de números complexos remonta ao século 16, mas só ganhou reconhecimento amplo e formalização rigorosa no século 19. Em programação de computadores, números complexos têm sido fundamentais para aritmética complexa em cálculos científicos e de engenharia desde os primórdios. A abordagem de Go para números complexos, tornando-os cidadãos de primeira classe com suporte embutido e suporte abrangente da biblioteca padrão através do pacote `math/cmplx`, destaca-se entre linguagens de programação. Esta decisão de design reflete a ênfase de Go na simplicidade e performance.

No entanto, vale ressaltar que trabalhar com números complexos em Go, embora poderoso, pode não ser sempre a melhor abordagem para todas as aplicações, particularmente aquelas que requerem matemática simbólica ou aritmética de alta precisão. Linguagens e ambientes especializados em computação científica, como Python com bibliotecas como NumPy e SciPy, ou softwares como MATLAB, podem oferecer mais flexibilidade e uma gama mais ampla de funcionalidades para aplicações específicas.

Dito isso, para programação de sistemas e contextos onde integrar cálculos de números complexos em uma aplicação maior e sensível à performance é crucial, o suporte nativo de Go para números complexos oferece uma opção eficientemente única.
