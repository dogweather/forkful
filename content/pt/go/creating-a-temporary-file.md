---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Go: Criando arquivos temporários rapidamente

## O Que e Por Que?
Criar um arquivo temporário em programação é a ação de designar um espaço de armazenamento em disco para armazenar dados de curta duração. Os programadores fazem isso para processar grandes quantidades de dados que não cabem na memória ou para armazenar informações que podem ser descartadas após o uso.

## Como Fazer:
Em Go, a biblioteca padrão `io/ioutil` fornece uma maneira simples de criar arquivos temporários. Vamos conferir um exemplo:

``` Go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    tempFile, err := ioutil.TempFile("","tempFile")
    if err != nil {
        log.Fatal(err)
    }

    defer tempFile.Close()

    fmt.Println("O arquivo temporário foi criado: ", tempFile.Name())
}
```

Ao executar este código, resultará na criação de um arquivo temporário no diretório definido com um nome único. O nome do arquivo será impresso na tela.

## Mergulho Profundo
No passado, os arquivos temporários eram comumente usados para manipular grandes quantidades de dados em computadores com memória limitada. Hoje em dia, eles ainda são úteis quando estamos trabalhando com dados que são grandes demais para caber na memória, ou quando queremos compartilhar dados entre diferentes processos.

Alternativas à criação de arquivos temporários incluem o uso de bancos de dados em memória, como o Redis, ou a criação de pipelines de dados, onde os dados são passados diretamente entre processos.

A função `ioutil.TempFile()` em Go cria um arquivo temporário de maneira segura, garantindo que não haja conflito de nome de arquivo e tratando de todas as nuances de permissões de arquivo e limpeza.

## Veja Também:
- Documentação oficial Go para `io/ioutil`: https://golang.org/pkg/io/ioutil/
- Mais sobre bancos de dados em memória e uso de pipelines de dados: [Redis In-Memory Database](https://redis.io/) e [Using Data Pipelines](https://www.tutorialspoint.com/data_pipeline/data_pipeline_introduction.htm)