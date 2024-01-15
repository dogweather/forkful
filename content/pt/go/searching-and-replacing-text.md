---
title:                "Busca e substituição de texto"
html_title:           "Go: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Muitas vezes, quando estamos trabalhando com código, precisamos fazer mudanças em partes específicas do texto. Isso pode ser um processo tedioso e demorado se for feito manualmente. Felizmente, com o Go, podemos usar a função de busca e substituição para fazer essas alterações de forma rápida e eficiente.

## Como Fazer
Para realizar uma busca e substituição com Go, usamos a função `ReplaceAllString()` da biblioteca `regexp`. Primeiro, precisamos importar a biblioteca em nosso código:

    ```Go
    import "regexp"
    ```
    
Em seguida, podemos definir nosso padrão de busca e o texto a ser substituído:

    ```Go
    pattern := "hello"
    replace := "ola"
    ```

Agora, podemos aplicar a função `ReplaceAllString()` em uma string de texto:

    ```Go
    newText := regexp.ReplaceAllString("hello world", pattern, replace)
    fmt.Println(newText) // output: ola world
    ```

Podemos também usar expressões regulares em nosso padrão de busca para tornar a busca mais flexível:

    ```Go
    pattern := "[a-z]+" // busca qualquer palavra contendo letras minúsculas
    replace := "text"
    newText := regexp.ReplaceAllString("Hello World", pattern, replace)
    fmt.Println(newText) // output: text text
    ```

## Aprofundando
Além da função `ReplaceAllString()`, a biblioteca `regexp` também possui outras funções úteis para busca e substituição de texto, como `ReplaceAll()`, `ReplaceAllLiteral()`, `ReplaceAllStringFunc()`, entre outras. Além disso, podemos usar expressões regulares mais avançadas para realizar buscas mais complexas e precisas. É importante ter conhecimento sobre expressões regulares para aproveitar ao máximo essa funcionalidade do Go.

## Veja Também
- [Documentação da biblioteca regexp](https://golang.org/pkg/regexp/)
- [Tutorial de expressões regulares no Go](https://www.golang-book.com/books/intro/12)