---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Escrever arquivos de texto é o ato de salvar dados em um formato legível por humanos. Programadores fazem isso para registrar informações, configurar programas ou salvar dados resultantes de operações.

## Como Fazer:
```Go
package main

import (
    "bufio"
    "os"
    "fmt"
)

func main() {
    arquivo, err := os.Create("exemplo.txt")
    if err != nil {
        fmt.Println("Erro ao criar arquivo:", err)
        return
    }
    defer arquivo.Close()

    escritor := bufio.NewWriter(arquivo)
    _, err = escritor.WriteString("Olá, arquivo de texto!\n")
    if err != nil {
        fmt.Println("Erro ao escrever no arquivo:", err)
        return
    }
    escritor.Flush()
}
```
Saída esperada: um arquivo com nome `exemplo.txt` contendo a linha "Olá, arquivo de texto!".

## Mergulho Profundo:
Historicamente, arquivos de texto são a base para a troca de dados entre programas e configuração de sistemas. Alternativas incluem bancos de dados e armazenamento na nuvem, mas arquivos de texto permanecem populares pela sua simplicidade e portabilidade. A escrita depende do sistema operacional para acessar o sistema de arquivos, com o Go fornecendo abstrações como o pacote "os" e "bufio" para facilitar estas operações.

## Ver Também:
- Documentação Go para o pacote "os": https://pkg.go.dev/os
- Documentação Go para o pacote "bufio": https://pkg.go.dev/bufio
- Tutorial Go sobre leitura e escrita de arquivos: https://gobyexample.com/reading-files
