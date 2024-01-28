---
title:                "Usando um depurador"
date:                  2024-01-26T03:49:08.020904-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## O quê e Por quê?
Usar um depurador é como ter um GPS na selva de código; ele te guia até a origem do problema. Programadores utilizam depuradores para percorrer seu código passo a passo, inspecionar variáveis e entender o fluxo, facilitando a identificação de bugs e a otimização de performance.

## Como fazer:
Go possui uma ferramenta integrada para depuração chamada Delve (`dlv`). Para começar, instale o Delve, escreva um programa simples em Go e depois execute-o através do depurador.

```Go
// Primeiro, instale o Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Exemplo de programa em Go, salve como main.go
package main

import "fmt"

func main() {
    message := "Depurando com Delve!"
    fmt.Println(message)
}

// Execute seu programa com o Delve
// dlv debug

// Alguns comandos básicos do Delve:
// (dlv) break main.main // define um ponto de interrupção na função main
// (dlv) continue // executa até o ponto de interrupção ou término do programa
// (dlv) step // avança passo a passo pelo programa
// (dlv) print message // imprime o valor atual da variável 'message'
// (dlv) quit // sai do Delve
```

Executar `dlv debug` inicia uma sessão de depuração. Uma vez que você atinge um ponto de interrupção definido, você pode percorrer seu programa e ver o que está acontecendo internamente.

## Aprofundamento
Historicamente, programadores Go usaram várias ferramentas para depuração, como o GDB (GNU Debugger), mas enfrentaram desafios por o GDB não ser adaptado para o tempo de execução e goroutines do Go. Delve veio em socorro com melhor suporte para as características únicas do Go.

Existem alternativas ao Delve, como `go-dbg`, e até suporte integrado de depurador em IDEs como Visual Studio Code e GoLand, que incorporam o Delve para uma experiência mais amigável ao usuário.

No lado da implementação, o Delve trabalha utilizando os pacotes `runtime` e `debug/gosym`, entre outros, para acessar e interpretar símbolos de programas Go e informações de execução. Ele é constantemente atualizado para acompanhar novas características da linguagem e versões.

## Veja também
- Repositório Oficial do Delve: https://github.com/go-delve/delve
- Tutorial de Depurador Go pela equipe Go: https://golang.org/doc/gdb
- Depuração Go no Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging
