---
title:                "Lendo argumentos da linha de comando"
html_title:           "Go: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Go?

Se você está começando a aprender a programar em Go, é importante entender como ler argumentos da linha de comando. Isso permitirá que você crie programas mais flexíveis e interativos, que possam receber entrada do usuário através da linha de comando.

## Como ler argumentos da linha de comando em Go?

Para ler argumentos da linha de comando em Go, podemos usar o pacote "os". Vamos primeiro importá-lo em nosso código:

```Go
import "os"
```

Em seguida, usamos a função "Args" do pacote "os" para obter uma slice de strings contendo todos os argumentos passados na linha de comando. Aqui está um exemplo:

```Go
args := os.Args
```

Para acessar argumentos específicos, podemos usar o índice da slice. Por exemplo, o primeiro argumento pode ser acessado com "args[0]". Agora, vamos ver um exemplo completo de um programa que lê argumentos da linha de comando e exibe a saída:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args

    // Exibe o primeiro argumento
    fmt.Println("Primeiro argumento:", args[0])

    // Exibe todos os argumentos
    fmt.Println("Todos os argumentos:", args)
}
```

Ao executar este programa com o comando "go run programa.go argumento1 argumento2", a saída será:

```
Primeiro argumento: programa.go
Todos os argumentos: [programa.go argumento1 argumento2]
```

## Detalhes sobre a leitura de argumentos da linha de comando em Go

Além da função "os.Args", podemos usar outras funções do pacote "os" para manipular argumentos da linha de comando em Go. Por exemplo, podemos usar a função "Getenv" para obter o valor de uma variável de ambiente baseado em uma chave.

Também podemos usar o pacote "flag" para ajudar a processar e validar argumentos da linha de comando com mais facilidade. Este pacote oferece recursos como a definição de flags com tipos específicos, tratamento de argumentos obrigatórios e ajuda ao usuário.

Em geral, a leitura de argumentos da linha de comando em Go é uma tarefa simples, mas que pode trazer muitos benefícios para nossos programas. É uma habilidade importante a ser dominada por todos os programadores Go.

## Veja também

- Documentação oficial do pacote "os" em Go: https://golang.org/pkg/os/
- Documentação oficial do pacote "flag" em Go: https://golang.org/pkg/flag/
- Documentação oficial do comando "go" para leitura de argumentos: https://golang.org/cmd/go/#hdr-Command_line_arguments