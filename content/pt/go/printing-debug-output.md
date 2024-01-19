---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?

A impressão de saída de depuração permite que os programadores monitorizem e analisem o comportamento de um programa durante o seu desenvolvimento e teste. A saída de depuração ajuda a identificar e corrigir erros ou "bugs".

## Como Fazer:

No Go, você pode usar o pacote "fmt" para exibir mensagens de depuração. Um exemplo simples:

```Go
package main

import "fmt"

func main() {
  fmt.Println("Isto é uma mensagem de depuração!")
}
```

A saída deste código será:

```
Isto é uma mensagem de depuração!
```

## Uma Olhadela Mais Profunda

O pacote "fmt" do Go é baseado na biblioteca printf do C, que existe desde a década de 1970 para formatar a saída. Uma alternativa ao "fmt" é o pacote "log", que tem uma funcionalidade adicional de timestamping. Além disso, o Go também suporta depuração no nível da linguagem com o uso do pacote "runtime/debug", que pode produzir rastreamentos de pilha e outras informações para fins de depuração.

## Veja Também

1. Documentação oficial do Go para pacote "fmt": https://golang.org/pkg/fmt/
2. Documentação oficial do Go para pacote "log": https://golang.org/pkg/log/
3. Documentação oficial do Go para pacote “runtime/debug”: https://golang.org/pkg/runtime/debug/