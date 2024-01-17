---
title:                "Extraindo substrings"
html_title:           "Go: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores o fazem?

Extrair substrings é uma tarefa comum em programação, onde uma parte de uma string é separada e armazenada em uma nova variável. Os programadores fazem isso para obter apenas a informação necessária de uma string maior, permitindo que a manipulem ou usem em outros cálculos.

## Como fazer:

```Go
// Defina uma string original
original := "O artigo é sobre extrair strings em Go"

// Use o pacote 'strings' para extrair uma substring
substring := strings.Split(original, "extrair")[1]

// Imprima a substring resultante
fmt.Println(substring)
```
Output: "strings em Go"

## Mais detalhes:

A prática de extrair substrings tem sido usada por programadores há muito tempo, com versões anteriores da linguagem Go como a C e C++. Alternativas para extrair substrings incluem utilizar operações de indexação de strings, no entanto, isso pode se tornar um processo mais trabalhoso para strings maiores.

O pacote "strings" em Go oferece funções específicas para extrair substrings com maior eficiência. Além disso, a linguagem permite manipulação e combinação de strings de forma simplificada, tornando a tarefa de extrair substrings ainda mais fácil.

## Veja também:

- Documentação oficial do pacote "strings" em Golang: https://golang.org/pkg/strings/
- Tutorial sobre extração de substrings em Go: https://www.golangprograms.com/how-to-extract-substring-from-string.html