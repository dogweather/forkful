---
title:                "Lendo um arquivo de texto"
html_title:           "Gleam: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Porque?

Ler um arquivo de texto simplesmente significa ler o conteúdo de um arquivo de texto em um programa de computador. Os programadores frequentemente precisam ler arquivos de texto para obter informações armazenadas neles, como configurações ou dados para processamento.

## Como fazer:

Você pode facilmente ler um arquivo de texto em Gleam usando a função `File.read` e passando o caminho do arquivo como um argumento. Aqui está um exemplo simples:

```gleam
let resultado = File.read("caminho/do/arquivo.txt")
```

O conteúdo do arquivo será armazenado na variável `resultado`, que pode ser usada mais tarde no seu programa.

## Mergulho Profundo:

A leitura de arquivos de texto é uma tarefa comum em programação e está presente em muitas linguagens de programação. Alternativas ao Gleam incluem Rust e Elixir, que também têm funções para ler arquivos de texto.

A implementação de `File.read` em Gleam utiliza as bibliotecas padrão do sistema operacional para realizar a leitura do arquivo. Isso significa que a função é altamente eficiente e pode lidar com arquivos grandes.

## Veja Também:

[Documentação oficial do Gleam sobre a função File.read](https://gleam.run/documentation/stdlib/#file-read)

[Livro online gratuito sobre programação funcional com Gleam](https://book.functinal.ch/03-reading-and-writing.html)