---
title:                "Escrevendo um arquivo de texto"
html_title:           "Go: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?

A escrita de arquivos de texto é uma tarefa comum em programação, e envolve a criação ou modificação de um arquivo que contém texto formatado. Os programadores normalmente realizam essa ação para armazenar dados importantes em um formato legível e portátil, que pode ser facilmente acessado e compartilhado entre diferentes plataformas e sistemas.

## Como fazer:

Para escrever um arquivo de texto em Go, podemos usar a função `WriteFile` do pacote `io/ioutil`. Primeiro, importe o pacote em seu código:

```Go
import "io/ioutil"
```

Em seguida, use a função `WriteFile` fornecendo o nome do arquivo que deseja criar ou modificar, o conteúdo a ser escrito e as permissões do arquivo:

```Go
err := ioutil.WriteFile("meu_arquivo.txt", []byte("Olá mundo!"), 0644)
```

Se o arquivo não existir, ele será criado. Já se o arquivo existir, seu conteúdo será substituído pelo novo conteúdo fornecido. A função `WriteFile` também retorna um erro caso algo dê errado, então é importante sempre verificar se ocorreu algum erro após a chamada da função.

## Mergulho profundo:

Em Go, a função `WriteFile` é apenas uma das várias opções para escrever arquivos de texto. Outra alternativa popular é o uso do pacote `bufio`, que fornece uma camada adicional de buffer para gravar dados com mais eficiência. Além disso, é importante lembrar que a escrita de arquivos de texto envolve operações de entrada e saída (I/O), o que significa que pode ser um processo mais lento do que trabalhar apenas com dados em memória.

## Veja também:

- Documentação oficial do pacote `io/ioutil` em Go: https://golang.org/pkg/io/ioutil/
- Documentação oficial do pacote `bufio` em Go: https://golang.org/pkg/bufio/
- Tutorial sobre escrita de arquivos em Go: https://gobyexample.com/writing-files