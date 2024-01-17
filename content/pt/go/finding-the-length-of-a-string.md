---
title:                "Encontrando o comprimento de uma string"
html_title:           "Go: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por que?
Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente ao lidar com inputs do usuário. Obter o tamanho da string é importante para validação de dados e para realizar operações específicas, como a iteração através dos caracteres da string.

## Como fazer:
```Go
func main() {
   myString := "Olá mundo!"
   
   // utilizamos a função len para obter o tamanho da string
   tamanho := len(myString)
   
   fmt.Println(tamanho) // saída: 11
}
```

## Deep Dive:
A função len, utilizada no exemplo acima, é uma função integrada do Go que retorna o tamanho de uma string ou array. Ela foi introduzida na versão 1 do Go e é amplamente utilizada pelos programadores dessa linguagem. Outra forma de obter o tamanho de uma string é utilizando a função len() do pacote "strings", mas essa opção está disponível apenas a partir da versão 1.2 do Go.

Existem outras linguagens de programação que possuem funções semelhantes para obter o tamanho de uma string, como o "strlen" em C e o "length" em Java. Porém, é importante notar que a função len do Go não considera caracteres multibyte, o que pode afetar o resultado em casos específicos.

## Veja também:
- [Pacote "strings" na documentação oficial do Go](https://golang.org/pkg/strings/)
- [Funções integradas do Go](https://golang.org/pkg/builtin/)