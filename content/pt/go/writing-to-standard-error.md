---
title:                "Go: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é um aspecto importante da programação em Go que muitas vezes é negligenciado. Isso permite aos desenvolvedores capturar e lidar com possíveis erros de forma mais eficiente, melhorando a qualidade e confiabilidade do código.

## Como fazer

Para escrever para o erro padrão em Go, você pode usar a função "fmt.Fprintf()" que permite escrever em qualquer fluxo, incluindo o erro padrão. Um exemplo de código seria o seguinte:

```go
package main

import (
    "fmt"
)

func main() {
    fmt.Fprintf(os.Stderr, "Erro encontrado: %s", err)
}
```

Neste exemplo, a função "Fprintf()" está sendo usada para escrever a mensagem de erro na saída padrão de erro. Isso permite aos desenvolvedores terem mais controle sobre o tratamento e exibição de erros em seus programas.

## Mergulho profundo

Ao escrever para o erro padrão, é importante entender a diferença entre usar o erro padrão e a saída padrão. Ao escrever na saída padrão, a mensagem será exibida para o usuário final, enquanto escrever no erro padrão é mais voltado para os desenvolvedores, pois é usado para depuração e identificação de problemas no código.

Além disso, é importante levar em conta o desempenho ao escrever para o erro padrão. Escrever para o erro padrão é um processo mais lento do que escrever para a saída padrão, portanto, é recomendado usar essa funcionalidade apenas quando necessário.

## Veja também

- [Documentação oficial sobre a função Fprintf](https://golang.org/pkg/fmt/#Fprintf)
- [Tutorial sobre como capturar e lidar com erros em Go](https://blog.golang.org/error-handling-and-go)
- [Guia para escrever código robusto em Go](https://blog.learngoprogramming.com/7-tips-to-write-clean-go-code-6af99cd0f4be)