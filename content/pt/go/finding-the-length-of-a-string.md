---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Encontrar a extensão de uma string significa saber quantos caracteres ela contém. Como programadores, fazemos isso para controlar e manipular eficientemente data do tipo string.

## Como Fazer:

Aqui está um exemplo em Go (versão atual) para obter o comprimento de uma string:

```Go
package main

import(
"fmt"
)

func main() {
  frase := "Go é divertido!"
  fmt.Println(len(frase))
}
```
Quando executar este código, você verá o seguinte output: 

```Output
14
```
Neste exemplo, a string "Go é divertido!" tem 14 caracteres, incluindo espaços e sinais de pontuação.

## Visão Detalhada:

Para entender melhor a função len(), aqui estão algumas informações adicionais:

1) Histórico: Nos primórdios da programação, calcular o tamanho de uma string era uma tarefa manual e trabalhosa. Com a evolução das linguagens de programação, a função len() se tornou uma inclusão padrão para essa tarefa.

2) Alternativas: Embora len() seja a escolha mais comum para encontrar o tamanho de uma string em Go, existem outras técnicas disponíveis para linguagens de programação diferentes, como o método length() em Java.

3) Detalhes da Implementação: A função len() em Go retorna o número de bytes em uma string, não o número de caracteres. Isso significa que ela funcionará corretamente para ASCII e algumas codificações UTF-8, mas pode retornar um resultado incorreto para strings que contenham caracteres não ASCII.

## Veja Também:

Para mais informações sinta-se à vontade para explorar esses links.

Documentação Go para Função len: https://golang.org/pkg/builtin/#len

Go, Strings e Bytes: https://blog.golang.org/strings