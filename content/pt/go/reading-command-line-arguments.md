---
title:    "Go: Lendo argumentos de linha de comando"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que

Ao escrever programas em Go, é importante entender como o sistema operacional interage com o código. Uma tarefa comum é a leitura dos argumentos da linha de comando. Entender como isso funciona pode ajudar a criar programas mais flexíveis e versáteis.

## Como Fazer

A linguagem Go possui uma biblioteca nativa para lidar com a leitura de argumentos da linha de comando: `flag`. Com ela, é possível definir as opções que o programa aceita, bem como seus valores padrão. Veja um exemplo abaixo:

```Go
package main

import (
    "flag"
    "fmt"
)

func main() {
    // Define as opções e seus valores padrão
    toUpper := flag.Bool("upper", false, "Transforma o texto em letras maiúsculas")
    num := flag.Int("num", 0, "Define um número inteiro")

    // Realiza a leitura dos argumentos da linha de comando
    flag.Parse()

    // Imprime os valores passados
    fmt.Println("upper:", *toUpper)
    fmt.Println("num:", *num)

    // Outro argumento que não foi definido é
    // impresso caso seja passado na linha de comando
    fmt.Println("args:", flag.Args())
}
```

Ao executar este programa com os argumentos `-upper -num 10`, teremos o seguinte resultado:

```
upper: true
num: 10
args: []
```

## Mergulho Profundo

Além de permitir a leitura de argumentos de linha de comando, a biblioteca `flag` também oferece suporte para a criação de opções de linha de comando mais complexas, como valores com múltiplos parâmetros e a possibilidade de escolher entre diferentes tipos de dados.

## See Also

- [Documentação oficial do pacote flag em Go](https://golang.org/pkg/flag/)
- [Artigo no blog do Go que explora a leitura de argumentos da linha de comando](https://blog.golang.org/using-go-flags)
- [Vídeo tutorial mostrando como usar o pacote flag em Go](https://www.youtube.com/watch?v=8gGgDIcqrx4)