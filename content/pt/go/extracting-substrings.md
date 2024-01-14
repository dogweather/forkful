---
title:                "Go: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em programas Go

Extrair substrings é uma tarefa comum em muitos programas Go. Isso permite que os desenvolvedores trabalhem com partes específicas de uma string, em vez de manipulá-la como um todo. Neste post, vamos explorar por que e como extrair substrings em Go, além de mergulhar mais fundo nessa funcionalidade da linguagem.

## Como fazer a extração de substrings em Go

A extração de substrings em Go é feita usando a função `substring()`, que recebe dois parâmetros: a string original e os índices inicial e final da substring desejada. Vamos ver um exemplo de como extrair a terceira palavra de uma frase:

```Go
texto := "Olá, este é um exemplo de frase"
palavras := strings.Split(texto, " ")  // separa as palavras
fmt.Println(palavras[2])  // extrai a terceira palavra
```

A saída desse código será "é", pois o índice da terceira palavra é 2. É importante lembrar que os índices em Go começam em 0, então a primeira palavra tem índice 0, a segunda tem índice 1 e assim por diante.

## Mergulho profundo na extração de substrings em Go

Além da função `substring()`, Go também oferece outras opções para extrair substrings, como `slice()` e `sprint()`. Além disso, é possível usar a combinação de métodos e funções para obter substrings mais complexas. Por exemplo, podemos usar a função `trim()` para remover espaços em branco extras de uma string antes de extrair uma substring.

É importante lembrar também que a extração de substrings pode ser uma operação custosa em termos de desempenho, especialmente ao lidar com strings muito grandes. Portanto, é sempre recomendável avaliar alternativas e otimizar o código, se necessário.

## Veja também

- Documentação oficial do Go sobre extração de substrings: https://golang.org/pkg/strings/#example_IndexAny
- Tutorial sobre manipulação de strings em Go: https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go-pt
- Discussão no fórum do Reddit sobre as diferentes maneiras de extrair substrings em Go: https://www.reddit.com/r/golang/comments/andr9n/extracing_substrings/