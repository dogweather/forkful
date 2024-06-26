---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:47.709242-07:00
description: "Como Fazer: Em Go, strings s\xE3o tratadas como sequ\xEAncias imut\xE1\
  veis de bytes. Voc\xEA pode encontrar o comprimento de uma string usando a fun\xE7\
  \xE3o embutida\u2026"
lastmod: '2024-03-13T22:44:46.050858-06:00'
model: gpt-4-0125-preview
summary: "Em Go, strings s\xE3o tratadas como sequ\xEAncias imut\xE1veis de bytes."
title: Encontrando o comprimento de uma string
weight: 7
---

## Como Fazer:
Em Go, strings são tratadas como sequências imutáveis de bytes. Você pode encontrar o comprimento de uma string usando a função embutida `len()`, que retorna o número de bytes, não necessariamente o número de caracteres. Veja como usá-la:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Usando len() para encontrar o comprimento em bytes
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Comprimento em Bytes:", byteLength) // Saída: Comprimento em Bytes: 13

	// Para obter com precisão o número de caracteres ou runas numa string
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Comprimento em Runas:", runeLength) // Saída: Comprimento em Runas: 9
}
```
O primeiro método usando `len()` pode não sempre fornecer o resultado esperado, uma vez que conta bytes. Para strings contendo caracteres não ASCII (como "世界"), deve-se usar `RuneCountInString` do pacote `unicode/utf8` para contar pontos de código Unicode de forma precisa.

## Aprofundamento
Antes do Go 1, não havia uma demarcação estrita para o tratamento de strings como sequências de bytes versus sequências de caracteres. Após o Go 1, a adoção do UTF-8 como esquema de codificação padrão para strings tornou necessárias abordagens mais claras. A função `len()` funciona perfeitamente para strings ASCII, onde os caracteres são representados em um único byte. No entanto, à medida que as aplicações Go se tornaram mais globais, e a necessidade de suportar uma miríade de idiomas e conjuntos de caracteres cresceu, a abordagem simplista de `len()` mostrou limitações.

A introdução e o uso de `utf8.RuneCountInString()` respondem a essas limitações fornecendo uma maneira de contar caracteres Unicode reais (runas na terminologia Go). Este método garante que o cálculo do comprimento seja independente das especificidades de codificação do UTF-8, onde os caracteres podem abranger vários bytes.

Uma abordagem alternativa para atravessar e manipular strings, mais alinhada com o ethos de concorrência e eficiência do Go, pode envolver o tratamento de strings como fatias de runas. No entanto, esse método necessita de uma etapa de conversão e não resolve instantaneamente todas as complexidades do Unicode (por exemplo, caracteres combinados).

Em resumo, enquanto `len()` é adequado para o comprimento em bytes e é eficiente para texto ASCII, `utf8.RuneCountInString()` é uma escolha mais confiável para uma aplicação compatível globalmente. Ainda assim, incentiva-se que os desenvolvedores entendam os compromissos em desempenho e uso de memória que essas escolhas implicam.
