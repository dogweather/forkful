---
title:                "Convertendo uma string para minúsculas"
html_title:           "Go: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

WHAT & WHY?

Converter uma string para letras minúsculas é um processo comum na programação, no qual todas as letras maiúsculas de uma string são transformadas em letras minúsculas. Os programadores fazem isso para garantir que a comparação de strings seja feita de maneira correta e consistente, independentemente do caso das letras.

HOW TO:

### Exemplo 1:

```Go
str := "Hello World"
str = strings.ToLower(str)
fmt.Println(str)
```

Saída: hello world

### Exemplo 2:

```Go
str := "GOLANG IS AWESOME"
str = strings.ToLower(str)
fmt.Println(str)
```

Saída: golang is awesome

### Exemplo 3:

```Go
str := "I <3 Go"
str = strings.ToLower(str)
fmt.Println(str)
```

Saída: i <3 go

DEEP DIVE:

Este processo de conversão de string para letras minúsculas pode ser útil em várias situações, especialmente quando se trabalha com entrada de usuário. Como nem sempre podemos confiar na entrada de dados dos usuários, é importante garantir que a comparação de strings seja feita de maneira precisa e consistente. Além disso, esse processo também é essencial em alguns algoritmos, como algoritmos de ordenação de strings, nos quais é necessário que as letras maiúsculas e minúsculas sejam tratadas da mesma forma.

Uma alternativa para o método "ToLower" seria o método "strings.ToUpper", que converte todas as letras de uma string em letras maiúsculas. No entanto, a escolha entre um método ou outro dependerá sempre da necessidade do programador.

Para implementar a conversão de string para letras minúsculas em Go, podemos utilizar a função "ToLower" da biblioteca "strings". Essa função percorre a string e, a cada letra maiúscula encontrada, substitui por sua correspondente em minúsculo.

SEE ALSO:

- [Documentação Go sobre a função ToLower](https://golang.org/pkg/strings/#ToLower)
- [Como usar a biblioteca strings em Go](https://www.golangprograms.com/golang-package-explained-string.html)