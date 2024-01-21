---
title:                "Concatenando strings"
date:                  2024-01-20T17:34:45.142236-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Concatenar strings significa juntar duas ou mais sequências de caracteres para formar uma nova. Programadores fazem isso para construir mensagens, combinar dados ou simplesmente formatar a saída.

## Como Fazer:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Concatenação simples com o operador +
	saudacao := "Olá, " + "mundo!"
	fmt.Println(saudacao) // Saída: Olá, mundo!

	// Usando a função Join do pacote strings para concatenar uma lista de strings
	palavras := []string{"Concatenar", "é", "divertido!"}
	frase := strings.Join(palavras, " ")
	fmt.Println(frase) // Saída: Concatenar é divertido!

	// Usando a função Sprintf para concatenar com formatação
	nome := "Gopher"
	idade := 10
	mensagem := fmt.Sprintf("O %s tem %d anos de Go!", nome, idade)
	fmt.Println(mensagem) // Saída: O Gopher tem 10 anos de Go!
}
```

## Mergulho Profundo:
Historicamente, a concatenação de strings tem sido uma operação fundamental em muitas linguagens de programação. Em Go, a operação é otimizada para evitar a criação desnecessária de objetos string intermediários, tornando o processo mais eficiente. Existem diversas maneiras de concatenar strings, como o operador `+`, que é direto mas pode ser custoso para a memória em loops. `strings.Builder` é outra alternativa poderosa para construções mais complexas, que evita o desperdício de memória. Por debaixo dos panos, quando você concatena strings usando o operador `+`, o compilador de Go pode otimizar as concatenações em loops para reduzir a alocação de memória e a pressão sobre o coletor de lixo.

## Veja Também:
- A documentação oficial da Go sobre strings: https://pkg.go.dev/strings
- Um artigo sobre a otimização de performance na concatenação de strings em Go: https://go.dev/blog/strings
- O pacote strings.Builder para eficiência em concatenações: https://pkg.go.dev/strings#Builder