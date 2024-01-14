---
title:                "Go: Lendo argumentos da linha de comando"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Porquê

Ao criar um programa em Go, muitas vezes é necessário interagir com o utilizador e receber entradas para possibilitar diferentes comportamentos do programa. Uma forma de realizar isso é através da leitura de argumentos da linha de comando. Neste artigo, vamos explorar como podemos ler e utilizar estes argumentos em um programa Go.

## Como Fazer

Para ler argumentos da linha de comando em um programa Go, podemos utilizar a função `os.Args`, que retorna uma slice contendo os argumentos fornecidos pelo utilizador. Podemos percorrer esta slice e utilizar os valores em nosso programa.

Para obter uma leitura mais organizada, podemos utilizar o pacote `flag`, que nos permite definir argumentos com diferentes tipos de valores e, em seguida, acessar esses valores através de variáveis. Por exemplo, podemos utilizar a função `flag.Int()` para definir um argumento que só aceita valores inteiros, e em seguida, utilizar a função `flag.Parse()` para atribuir o valor fornecido pelo utilizador à nossa variável.

Veja um exemplo de como podemos utilizar a função `flag` para ler e utilizar argumentos da linha de comando em um programa Go:

```Go
package main

import (
    "flag"
    "fmt"
)

func main() {
    nome := flag.String("nome", "utilizador", "Escreva o seu nome.")
    idade := flag.Int("idade", 18, "Escreva a sua idade.")

    // É necessário chamar a função `flag.Parse()` para que os valores
    // fornecidos pelo utilizador sejam atribuídos às variáveis.
    flag.Parse()

    fmt.Printf("Olá, %s! Você tem %d anos.\n", *nome, *idade)
}
```

Se executarmos este programa com o comando `go run main.go --nome João --idade 25`, por exemplo, veremos a seguinte saída:

```
Olá, João! Você tem 25 anos.
```

## Aprofundando

Além da função `os.Args` e do pacote `flag`, existem outras formas de ler argumentos da linha de comando em Go, como utilizar o pacote `os` para acessar diretamente as variáveis de ambiente, ou utilizar uma biblioteca de terceiros como o `cobra`.

Também é importante ressaltar que a ordem dos argumentos pode ser importante em certos casos, e que existem convenções para utilização de flags (argumentos com opções) e parâmetros (argumentos sem opções).

## Veja Também

- [Documentação oficial do pacote `flag`](https://pkg.go.dev/flag)
- [Tutorial sobre a leitura de argumentos da linha de comando em Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-golang)
- [Outras formas de ler argumentos da linha de comando em Go](https://www.callicoder.com/golang-command-line-arguments/#alternative-ways-to-read-command-line-arguments)

Obrigado por ler este artigo! Espero que tenha sido útil para você utilizar argumentos da linha de comando em seus programas Go. Até a próxima!