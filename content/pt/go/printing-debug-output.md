---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:52:44.524345-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
"Debug output" é o ato de imprimir informações no console para entender o que tá rolando no código. Programadores fazem isso para rastrear bugs e monitorar o fluxo dos programas.

## Como Fazer:
```Go
package main

import (
    "fmt"
    "log"
    "os"
)

func main() {
    // Usando fmt
    fmt.Println("Isto é uma mensagem de debug usando fmt")

    // Usando log, com data e hora
    log.Println("Isto é uma mensagem de debug usando log")

    // Configurando o log para um arquivo
    file, err := os.OpenFile("debug.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()
    log.SetOutput(file)
    log.Println("Isto foi salvo no arquivo debug.log")
}
```
Sample output:
```
Isto é uma mensagem de debug usando fmt
2023/01/02 15:04:05 Isto é uma mensagem de debug usando log
```
## Mergulho Profundo
Antes do `fmt` e `log`, a galera imprimia mensagem na tela com coisas como `printf` em linguagens tipo C. No Go, `fmt` é simples, sem frescura, mas não registra data/hora. Já `log` joga essa informação junto, o que é uma mão na roda para rastrear o histórico. Além do mais, com `log` você pode direcionar o output para um arquivo, se quiser guardar os registros.

Alternativas incluem pacotes de terceiros com mais recursos, como o `zap` ou `logrus`, que oferecem estruturação de logs em JSON, filtragem de níveis de log e mais.

Os detalhes importantes de implementação: ao usar `log.SetOutput`, você muda onde os logs são escritos – podem ser arquivos, buffers ou qualquer coisa que implemente a interface `io.Writer`.

## Veja Também
- Documentação Go para o pacote fmt: https://pkg.go.dev/fmt
- Documentação Go para o pacote log: https://pkg.go.dev/log
- Uma comparação de pacotes de log em Go: https://medium.com/@nate510/don-t-use-go-s-default-logger-4804cb19f779
- Repo de um logger popular, Logrus: https://github.com/sirupsen/logrus
- Repo de um logger de alta performance, Zap: https://github.com/uber-go/zap
