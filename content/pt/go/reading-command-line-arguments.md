---
title:    "Go: Lendo argumentos da linha de comando"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Go?

Ao escrever um programa em Go, muitas vezes é necessário fornecer ao seu programa algum tipo de entrada ou configuração. Uma maneira de fazer isso é através de argumentos da linha de comando. Ao aprender a ler e usar esses argumentos, você pode tornar seu programa mais dinâmico e flexível.

## Como fazer: Lendo argumentos da linha de comando em Go

Ler argumentos da linha de comando em Go é uma tarefa relativamente simples. Primeiro, você precisará importar o pacote "os" para ter acesso aos argumentos fornecidos ao programa. Em seguida, você pode usar a função "os.Args" para obter uma lista de todos os argumentos passados ao programa. Aqui está um exemplo de um programa que lê e imprime os argumentos fornecidos:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("Argumentos fornecidos:", os.Args[1:])
}
```

Ao executar este programa a partir da linha de comando, você pode fornecer argumentos após o nome do programa. Por exemplo, se o programa for chamado de "meu_programa", você pode executá-lo da seguinte maneira:

`./meu_programa argumento1 argumento2`

O resultado seria:

```
Argumentos fornecidos: [argumento1 argumento2]
```

## Mergulho Profundo: Explorando os argumentos da linha de comando em Go

Você não está limitado a apenas obter uma lista de argumentos com "os.Args". Você também pode acessar argumentos individuais usando a sintaxe "args[index]". Além disso, é possível verificar a quantidade de argumentos fornecidos usando a função "len". Aqui está um exemplo de como você pode fazer isso:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("Número de argumentos fornecidos:", len(os.Args)-1)
    fmt.Println("O primeiro argumento fornecido foi:", os.Args[1])
    fmt.Println("O segundo argumento fornecido foi:", os.Args[2])
    // E assim por diante, dependendo do número de argumentos fornecidos.
}
```

Experimente com diferentes argumentos e veja como o programa se comporta. Você também pode usar um loop for para imprimir todos os argumentos fornecidos ao programa.

## Veja também

- [Documentação do pacote "os" em Go](https://golang.org/pkg/os/#Args)
- [Tutorial sobre como usar os.Args em Go](https://www.golangprograms.com/command-line-arguments.html)
- [Vídeo tutorial sobre como ler argumentos da linha de comando em Go](https://www.youtube.com/watch?v=J_EBI4ssDeI)