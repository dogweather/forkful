---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Argumentos de linha de comando são dados que você envia para o seu programa no momento de sua execução. Programadores normalmente lêem argumentos de linha de comando para manipular comportamentos específicos do programa sem alterar o código.

## Como fazer:

Vamos começar com um exemplo simples:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argumentos := os.Args
	fmt.Println(argumentos)
}
```

Você pode executar esse programa com alguns argumentos para ver o resultado:

```bash
go run main.go argumento1 argumento2
```

O resultado será algo como: `[/tmp/go-build123456789/b001/exe/main argumento1 argumento2]`.

## Aprofundamento

Das ferramentas de linha de comando do Unix aos modernos aplicativos de servidor, a prática de leitura de argumentos de linha de comando tem um papel significativo na programação. Ela fornece uma forma conveniente e flexível de controlar o comportamento do programa.

Existem alternativas para ler argumentos de linha de comando, como usar arquivos de configuração ou variáveis de ambiente. No entanto, eles funcionam melhor para diferentes cenários e não são mutuamente exclusivos.

Por baixo dos panos, a biblioteca padrão `os` no Go fornece a variável global `Args` que armazena todos os argumentos de linha de comando. Vale a pena notar que `os.Args` inclui o nome do programa como o primeiro argumento, e é por isso que muitas vezes você vê `os.Args[1:]` para acessar apenas os argumentos passados pelo usuário.

## Veja também

1. Documentação oficial do Go: [os package](https://golang.org/pkg/os)
2. Tutorial de Go para iniciantes: [Command line arguments](https://gobyexample.com/command-line-arguments)
3. Stack Overflow: [How to parse command line arguments](https://stackoverflow.com/questions/8372399/how-to-parse-command-line-arguments-in-go)