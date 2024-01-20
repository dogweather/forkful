---
title:                "Escrevendo para o erro padrão"
html_title:           "Go: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever para o erro padrão é uma técnica usada pelos programadores para exibir mensagens de erro e diagnósticos durante a execução de um programa. Isso é especialmente útil quando o programa precisa informar ao usuário sobre um erro inesperado ou falha no processo.

## Como fazer:

Em Go, você pode escrever para o erro padrão usando a função `fmt.Fprintln()`. Isso aceita dois argumentos: o primeiro é o destino de saída, que deve ser `os.Stderr` para escrever no erro padrão; e o segundo é a mensagem que você deseja exibir. Aqui está um exemplo simples:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintln(os.Stderr, "Algo deu errado!")
}
```

A saída deste programa seria `Algo deu errado!` no erro padrão.

## Mergulho Profundo:

A prática de escrever para o erro padrão remonta aos primórdios da programação e é uma maneira eficaz de ajudar os usuários a resolver problemas e entender o comportamento do programa. No entanto, há alternativas, como escrever em um arquivo de log ou usar bibliotecas de registro mais avançadas em Go, como o pacote `log`.

No Go, a saída para o erro padrão é implementada usando a interface `io.Writer`, que permite que os dados sejam escritos em qualquer lugar que implemente essa interface, como um arquivo ou a saída padrão. É importante notar que a saída padrão e o erro padrão são dois fluxos diferentes e geralmente não devem ser confundidos.

## Veja também:

Para saber mais sobre a escrita para o erro padrão em Go, confira as seguintes fontes:

- [A documentação oficial da função `fmt.Fprintln()` em Go.](https://golang.org/pkg/fmt/#Fprintln)
- [Um exemplo de uso do pacote `log` para registro avançado em Go.](https://medium.com/@deadkarma/logging-in-golang-the-right-way-b0a90a1f7a8d)