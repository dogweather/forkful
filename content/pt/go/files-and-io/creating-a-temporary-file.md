---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:26.288743-07:00
description: "Criar um arquivo tempor\xE1rio em Go permite a gera\xE7\xE3o de um arquivo\
  \ n\xE3o persistente projetado para uso de curto prazo, principalmente para tarefas\
  \ como\u2026"
lastmod: '2024-03-13T22:44:46.081770-06:00'
model: gpt-4-0125-preview
summary: "Criar um arquivo tempor\xE1rio em Go permite a gera\xE7\xE3o de um arquivo\
  \ n\xE3o persistente projetado para uso de curto prazo, principalmente para tarefas\
  \ como\u2026"
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## O Que & Por Que?

Criar um arquivo temporário em Go permite a geração de um arquivo não persistente projetado para uso de curto prazo, principalmente para tarefas como armazenamento de dados intermediários ou auxílio em trabalhos de processamento em lote. Os programadores utilizam essa funcionalidade para manipular dados com segurança sem afetar o sistema de arquivos permanente ou necessitar de limpeza manual.

## Como Fazer:

No Go, o pacote `ioutil` originalmente fornecia utilidades para a criação de arquivos temporários. Contudo, o Go 1.16 promoveu o uso das funções dos pacotes `os` e `io/ioutil` para lugares mais organizados. Agora, os pacotes `os` e `io` são preferidos para manipulação de arquivos temporários.

Aqui está um guia passo a passo para criar, escrever e excluir um arquivo temporário:

1. **Criar um Arquivo Temporário:**

Usando a função `os.CreateTemp`, você pode criar um arquivo temporário. Sem especificar um diretório, ele usa a pasta temp padrão do seu sistema operacional.

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "example.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Arquivo temporário criado: %s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // Limpeza
}
```

2. **Escrever no Arquivo Temporário:**

Escrever no arquivo pode ser alcançado com o método `Write` ou outras funções de escrita dos pacotes `io` ou `bufio`.

```go
_, err = tmpFile.Write([]byte("Hello, World!"))
if err != nil {
    log.Fatal(err)
}
```

3. **Ler do Arquivo Temporário:**

A leitura segue de maneira similar, utilizando o método `Read` do arquivo, ou usando utilidades dos pacotes `io` ou `bufio`.

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("Dados lidos: %s\n", string(data))
```

4. **Excluir o Arquivo Temporário:**

Enquanto a declaração `defer os.Remove(tmpFile.Name())` na fase de criação garante que o arquivo temporário seja excluído após o término do programa, a exclusão explícita pode ser gerenciada conforme necessário.

Saída de Exemplo:
```
2023/04/01 15:00:00 Arquivo temporário criado: /tmp/example.123456.txt
2023/04/01 15:00:00 Dados lidos: Hello, World!
```

## Aprofundamento

O mecanismo por trás do manejo de arquivos temporários pelo Go evoluiu. Inicialmente, a criação de arquivos temporários era predominantemente gerenciada pela agora obsoleta função `ioutil.TempFile`, refletindo tendências mais amplas no desenvolvimento de software em direção a práticas de manipulação de arquivos mais seguras e eficientes. A mudança para integrar essas funcionalidades nos pacotes `os` e `io` com o Go 1.16 sinaliza um impulso mais amplo em direção à racionalização da biblioteca padrão da linguagem e encorajando o uso de APIs mais unificadas e coesas.

Embora o uso de arquivos temporários seja uma prática comum e muitas vezes essencial na programação, é importante notar que depender muito deles para armazenar grandes quantidades de dados ou para tarefas de longo prazo pode levar a problemas de desempenho. Além disso, quando a criação de arquivos temporários não é rigorosamente controlada ou quando eles não são adequadamente limpos, isso pode levar a vazamentos de recursos que poderiam impactar negativamente o sistema de arquivos. Em cenários que exigem armazenamento persistente ou requerem o manejo de fluxos de dados substanciais, alternativas como bancos de dados ou armazenamentos de dados em memória geralmente oferecem melhor desempenho e confiabilidade em comparação a arquivos temporários.
