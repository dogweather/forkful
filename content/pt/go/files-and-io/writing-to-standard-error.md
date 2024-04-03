---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:15.929272-07:00
description: "Escrever no erro padr\xE3o (stderr) em Go envolve direcionar mensagens\
  \ de erro ou diagn\xF3sticos que n\xE3o s\xE3o destinados ao fluxo de sa\xEDda principal.\
  \ Os\u2026"
lastmod: '2024-03-13T22:44:46.078667-06:00'
model: gpt-4-0125-preview
summary: "Escrever no erro padr\xE3o (stderr) em Go envolve direcionar mensagens de\
  \ erro ou diagn\xF3sticos que n\xE3o s\xE3o destinados ao fluxo de sa\xEDda principal."
title: "Escrevendo no erro padr\xE3o"
weight: 25
---

## O que & Por quê?

Escrever no erro padrão (stderr) em Go envolve direcionar mensagens de erro ou diagnósticos que não são destinados ao fluxo de saída principal. Os programadores usam isso para separar a saída regular das informações de erro, tornando a depuração e a análise de logs mais diretas.

## Como Fazer:

No Go, o pacote `os` fornece o valor `Stderr`, representando o arquivo de erro padrão. Você pode usá-lo com as funções `fmt.Fprint`, `fmt.Fprintf` ou `fmt.Fprintln` para escrever no stderr. Aqui está um exemplo simples:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Escrevendo uma string simples no stderr
    _, err := fmt.Fprintln(os.Stderr, "Esta é uma mensagem de erro!")
    if err != nil {
        panic(err)
    }

    // Mensagem de erro formatada com Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Processo concluído com %d erros.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Saída de exemplo (para stderr):
```
Esta é uma mensagem de erro!
Processo concluído com 4 erros.
```

Lembre-se, estas mensagens não aparecerão na saída regular (stdout), mas no fluxo de erro, que pode ser redirecionado separadamente na maioria dos sistemas operacionais.

## Aprofundando

O conceito de erro padrão está profundamente enraizado na filosofia Unix, que distingue claramente entre saída normal e mensagens de erro para um processamento e manipulação de dados mais eficientes. No Go, essa convenção é adotada através do pacote `os`, que fornece acesso direto aos descritores de arquivo stdin, stdout e stderr.

Embora escrever diretamente no `os.Stderr` seja adequado para muitas aplicações, o Go também oferece pacotes de registro mais sofisticados como `log`, que oferecem recursos adicionais, como carimbos de data/hora e configurações de saída mais flexíveis (por exemplo, escrever em arquivos). Usar o pacote `log`, especialmente para aplicações maiores ou onde são necessários recursos de registro mais abrangentes, pode ser uma alternativa melhor. Vale também mencionar que a abordagem do Go para o tratamento de erros, que incentiva a devolução de erros pelas funções, complementa a prática de escrever mensagens de erro no stderr, permitindo um controle mais granular do gerenciamento e relatório de erros.

Em essência, embora escrever no stderr seja uma tarefa fundamental em muitas linguagens de programação, a biblioteca padrão do Go e os princípios de design oferecem caminhos tanto simples quanto avançados para o gerenciamento da saída de erros, alinhando-se com as práticas mais amplas da indústria e também atendendo ao ethos de design específico do Go.
