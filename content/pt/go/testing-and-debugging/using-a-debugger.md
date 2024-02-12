---
title:                "Usando um depurador"
aliases:
- pt/go/using-a-debugger.md
date:                  2024-02-03T18:10:09.023739-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Utilizar um depurador na programação em Go envolve o uso de ferramentas ou recursos para inspecionar e modificar o estado de um programa em execução para entender seu comportamento ou diagnosticar problemas. Os programadores fazem isso para encontrar e corrigir bugs de forma eficiente, otimizar o desempenho e garantir a correção de seu código.

## Como fazer:

Go oferece uma facilidade integrada para depuração chamada `delve`. É uma ferramenta de depuração completa que permite executar programas em Go passo a passo, inspecionar variáveis do programa e avaliar expressões.

Para começar, você deve primeiro instalar o `delve`. Você pode fazer isso executando:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Agora, vamos depurar um programa Go simples. Considere um programa `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Depurando em Go"
    fmt.Println(message)
}
```

Para começar a depuração deste programa, abra um terminal no diretório do projeto e execute:

```shell
dlv debug
```

Este comando compila o programa com otimizações desabilitadas (para melhorar a experiência de depuração), inicia-o e anexa um depurador a ele.

Uma vez que o `delve` está em execução, você está no shell interativo do depurador. Aqui estão alguns comandos básicos:

- `break main.main` define um ponto de interrupção na função `main`.
- `continue` retoma a execução do programa até que um ponto de interrupção seja atingido.
- `print message` imprimirá o valor da variável `message`.
- `next` avança a execução do programa para a próxima linha.
- `quit` sai do depurador.

A saída ao atingir o ponto de interrupção e imprimir a variável pode parecer assim:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Depurando em Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Depurando em Go"
```

Usando esses comandos, você pode percorrer seu programa passo a passo, inspecionando o estado à medida que avança para entender como ele se comporta e identificar quaisquer problemas.

## Aprofundamento

A escolha do `delve` como a ferramenta de depuração de eleição para Go, em vez de ferramentas tradicionais como o GDB (GNU Debugger), deve-se principalmente à natureza do modelo de execução e do tempo de execução do Go. O GDB não foi inicialmente projetado com o tempo de execução do Go em mente, tornando o `delve` uma escolha mais adequada para desenvolvedores em Go. `Delve` é projetado especificamente para Go, oferecendo uma experiência de depuração mais intuitiva para rotinas Go, canais e outros construtos específicos do Go.

Além disso, `delve` suporta uma ampla gama de recursos além daqueles oferecidos pelo GDB básico ao trabalhar com programas Go. Estes incluem, mas não estão limitados a: anexar a processos em execução para depuração; pontos de interrupção condicionais; e avaliar expressões complexas que podem envolver primitivos de concorrência do Go.

Embora `delve` seja o depurador preferido de muitos desenvolvedores Go, vale ressaltar que a cadeia de ferramentas Go também inclui formas mais leves de suporte à depuração, como a ferramenta integrada `pprof` para perfilamento e a ferramenta `trace` para visualização de concorrência. Essas ferramentas às vezes podem fornecer uma via mais rápida ou de nível mais alto para diagnosticar problemas de desempenho do programa ou bugs de concorrência, o que pode ser complementar ou até preferível, dependendo do contexto de depuração.
