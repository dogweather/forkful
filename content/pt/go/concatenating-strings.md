---
title:                "Unindo strings"
html_title:           "Go: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Você já se deparou com a necessidade de juntar várias strings em uma única? Seja para formatar uma saída de dados ou para construir uma URL dinâmica, essa é uma tarefa comum durante o desenvolvimento de software. Felizmente, o Go possui uma solução simples e eficiente para esse problema, o que torna a concatenação de strings uma tarefa rápida e fácil de ser realizada.

## Como Fazer

Para concatenar strings em Go, podemos usar o operador "+" ou a função "fmt.Sprintf()". Vamos ver alguns exemplos utilizando ambas as formas:

```
package main

import "fmt"

func main() {
	// Utilizando o operador "+"
	firstName := "João"
	lastName := "Silva"
	fullName := firstName + " " + lastName
	fmt.Println(fullName) // Imprime "João Silva"

	// Utilizando a função "fmt.Sprintf()"
	id := 1234
	url := fmt.Sprintf("www.example.com/users/%d", id)
	fmt.Println(url) // Imprime "www.example.com/users/1234"
}
```

Neste primeiro exemplo, criamos duas strings `firstName` e `lastName` e utilizamos o operador "+" para concatená-las e armazenar o resultado na variável `fullName`. Em seguida, imprimimos essa variável na tela. Note que podemos inserir o espaço em branco entre as strings diretamente dentro do operador.

No segundo exemplo, criamos uma variável `id` do tipo inteiro e utilizamos a função "fmt.Sprintf()" para formatar uma URL dinâmica, inserindo o valor de `id` na string. O resultado é armazenado na variável `url` e posteriormente impresso na tela.

## Mergulho Profundo

É importante lembrar que o operador "+" pode ser utilizado não apenas com strings, mas também com outros tipos de dados básicos, como inteiros e floats. No entanto, é necessário ter cuidado ao concatenar strings com valores de outros tipos, pois isso pode causar erros ou resultados inesperados.

Além disso, é possível concatenar mais de duas strings utilizando o operador "+", por exemplo: `str1 + str2 + str3`. Porém, esse pode não ser o método mais eficiente, especialmente se precisarmos fazer várias concatenações. Isso porque, a cada operação, uma nova string é criada, sendo necessário alocar memória para armazená-la.

Uma opção mais eficiente é utilizar a função "fmt.Sprintf()" ou a função "strings.Join()" do pacote "strings". A função "fmt.Sprintf()" permite formatar uma string com placeholders, semelhante à função "printf()" da linguagem C. Já a função "strings.Join()" permite juntar múltiplas strings em uma única, especificando o separador entre elas.

## Veja também
- Documentação oficial do Go: https://golang.org/doc/
- Tutorial de Strings em Go: https://www.tutorialspoint.com/go/go_string_concatenation.htm
- Pacote "strings" do Go: https://golang.org/pkg/strings/