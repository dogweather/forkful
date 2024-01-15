---
title:                "Encontrando o tamanho de uma string"
html_title:           "Go: Encontrando o tamanho de uma string"
simple_title:         "Encontrando o tamanho de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string em Go

Muitas vezes, ao trabalhar com strings em um programa em Go, pode ser necessário calcular o seu comprimento. Isso pode ser útil, por exemplo, ao validar a entrada de um usuário ou manipular dados em um banco de dados. Felizmente, a linguagem Go oferece uma maneira simples de encontrar o comprimento de uma string. Neste artigo, vamos explorar como fazer isso.

## Como fazer

Para encontrar o comprimento de uma string em Go, usamos a função `len()`, que é nativa da linguagem. Essa função retorna o número de bytes que compõem a string. Vamos ver um exemplo prático:

```
nome := "Maria"
comprimento := len(nome)
fmt.Println(comprimento) // Output: 5
```

No exemplo acima, declaramos uma variável `nome` com o valor "Maria" e, em seguida, usamos a função `len()` para encontrar o seu comprimento, que é 5. É importante lembrar que, em Go, uma string é uma sequência de bytes, não um caractere como em outras linguagens.

Também podemos usar a função `len()` em strings multibyte, como caracteres acentuados ou emojis. Por exemplo:

```
mensagem := "Olá 😊"
comprimento := len(mensagem)
fmt.Println(comprimento) // Output: 6
```

## Aprofundando-se

Se você está se perguntando por que a função `len()` retorna o número de bytes e não o número de caracteres, é porque em Go, uma string é um tipo de dados imutável. Isso significa que cada caractere em uma string é armazenado como um byte individual. Isso torna a manipulação de strings mais eficiente em termos de desempenho e também evita problemas de codificação.

Outro ponto importante é que a função `len()` não conta o número de palavras de uma string, apenas os bytes. Por isso, se você quiser encontrar o número de palavras em uma string, é necessário separá-la em uma array de strings e usar a função `len()` nessa array.

Além disso, a função `len()` também pode ser usada em outros tipos de dados, como arrays, slices e maps. Experimente e veja como ela se comporta em cada um desses tipos!

## Veja também

- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial sobre strings em Go no site Learn Go](https://www.learn-golang.org/string)
- [Artigo em inglês sobre a função len() em Go](https://www.digitalocean.com/community/tutorials/how-to-find-the-length-of-a-string-in-go)