---
title:    "Go: Utilizando expressões regulares"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Go?

Se você é um programador Go, provavelmente já ouviu falar em expressões regulares. Mas por que alguém se incomodaria em aprender e usar essa ferramenta? A resposta é simples: expressões regulares são extremamente úteis quando se trata de localizar e manipular padrões de texto.

## Como usar expressões regulares em Go

Agora que já sabemos o porquê, vamos aprender a usar expressões regulares em Go. Primeiro, é necessário importar o pacote de expressões regulares com o comando `import "regexp"`. Em seguida, podemos usar a função `MatchString(pattern, input)` para procurar um padrão de texto específico em uma string. Por exemplo:

```Go
import "regexp"

func main() {
    match, _ := regexp.MatchString("go", "Golang é uma ótima linguagem de programação!")
    fmt.Println(match)
}
```

Neste exemplo, usamos a função `MatchString` para verificar se a string contém o padrão "go", e o resultado será `true` já que a string contém essa palavra. 

## Aprofundando em expressões regulares

Existem diversos recursos e opções disponíveis ao usar expressões regulares em Go. Por exemplo, podemos usar os caracteres `^` e `$` para delimitar o início e o fim de uma string. Também podemos usar o caractere `*` para indicar que um determinado padrão deve repetir 0 ou mais vezes. Há ainda muitas outras possibilidades, e é importante estudar a documentação oficial do pacote `regexp` para entender todas as opções disponíveis.

## Veja também
- [Documentação do pacote regexp em Go](https://golang.org/pkg/regexp/)
- [Cheat sheet de expressões regulares em Go](https://yourbasic.org/golang/regexp-cheat-sheet/)
- [Tutorial de expressões regulares em Go](https://www.calhoun.io/learning-golang-regexp/)