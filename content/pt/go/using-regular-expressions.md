---
title:    "Go: Utilizando expressões regulares"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Go?

As expressões regulares são uma poderosa ferramenta para manipulação de strings e padrões em código. Elas podem ser úteis em diversas situações, como validação de dados, busca e substituição de strings, e até mesmo na criação de parsers. Em Go, é possível utilizar a biblioteca built-in "regexp" para trabalhar com expressões regulares.

## Como Usar

Para utilizar expressões regulares em Go, primeiro é necessário importar a biblioteca "regexp". Em seguida, podemos utilizar a função "regexp.Compile" para compilar nossa expressão regular e retornar uma estrutura que pode ser utilizada para fazer a correspondência de padrões.

```Go
import "regexp"

func main() {
  // Compila a expressão regular
  re := regexp.MustCompile("h(o|i)s")

  // Faz a correspondência com a string "his" 
  fmt.Println(re.MatchString("his")) // true

  // Faz a correspondência com a string "hers"
  fmt.Println(re.MatchString("hers")) // true

  // Não faz a correspondência com a string "hat"
  fmt.Println(re.MatchString("hat")) // false
}
```

## Aprofundamento

Existem diversas opções e métodos disponíveis para trabalhar com expressões regulares em Go. Alguns desses métodos incluem "FindString", que encontra a primeira correspondência de padrão em uma string, e "FindAllString", que encontra todas as correspondências de padrão em uma string.

Para uma lista completa dos métodos disponíveis, consulte a documentação oficial da biblioteca "regexp" em https://golang.org/pkg/regexp/.

## Veja também

- [Tutorial de Expressões Regulares em Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pt)
- [Documentação Oficial da Biblioteca "regexp"](https://golang.org/pkg/regexp/)
- [Curso Gratuito de Expressões Regulares em Go](https://www.udemy.com/course/aprenda-expressoes-regulares-em-golang/)