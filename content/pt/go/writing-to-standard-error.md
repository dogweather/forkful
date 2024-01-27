---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever no erro padrão é a prática de enviar mensagens de erro para um canal específico, diferentemente da saída padrão. Programadores fazem isso para separar os erros dos dados normais de saída, facilitando o diagnóstico e a depuração.

## How to:
Para escrever no erro padrão em Go, utilize o pacote `os` e `fmt`. Exemplo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Open("nonexistent.file")
    if err != nil {
        fmt.Fprintf(os.Stderr, "Erro ao abrir arquivo: %v\n", err)
    }
}
```

Saída de exemplo:

```
Erro ao abrir arquivo: open nonexistent.file: no such file or directory
```

## Deep Dive
O conceito de erro padrão vem de sistemas Unix, onde era uma das três principais streams de dados, junto com a entrada padrão e saída padrão. Alternativas incluem o log de erros em arquivos ou encaminhamento para serviços de monitoramento. A escrita para o erro padrão é implementada no Go através do pacote `os`, onde `os.Stderr` é um `*File` apontando para a saída de erro padrão do sistema operacional.

## See Also
- Documentação do Go para o pacote `fmt`: https://pkg.go.dev/fmt
- Documentação do Go para o pacote `os`: https://pkg.go.dev/os
- Artigo sobre streams na Unix: https://en.wikipedia.org/wiki/Standard_streams
