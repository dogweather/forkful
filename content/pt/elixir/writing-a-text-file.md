---
title:                "Escrevendo um arquivo de texto"
html_title:           "Elixir: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Escrever um arquivo de texto é o processo de criar um arquivo contendo somente texto puro, sem nenhuma formatação especial. Programadores frequentemente escrevem arquivos de texto para armazenar dados simples, como nomes de usuários ou configurações de aplicativos.

## Como Fazer:

```elixir
# Abrindo um arquivo para escrita
arquivo = File.open("meu_arquivo.txt", [:write])

# Escrevendo uma linha de texto
File.write(arquivo, "Olá, mundo!")

# Escrevendo múltiplas linhas de texto
File.write(arquivo, "Primeira linha\nSegunda linha\nTerceira linha")

# Fechando o arquivo
File.close(arquivo)
```

## Deep Dive:

Escrever arquivos de texto é um recurso muito útil e simples em programação, e tem sido usado há décadas. Existem muitas alternativas, com diferentes linguagens e bibliotecas oferecendo maneiras diferentes de escrever arquivos de texto. O Elixir, em particular, possui a função `File.write` que facilita muito esse processo.

## Veja Também:

- [Documentação do Elixir sobre a função File.write](https://hexdocs.pm/elixir/File.html#write/2)
- [Exemplos de escrita de arquivos em Elixir](https://stackoverflow.com/questions/3222970/how-to-write-text-file-using-elixir)