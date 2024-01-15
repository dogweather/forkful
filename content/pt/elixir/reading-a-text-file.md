---
title:                "Lendo um arquivo de texto"
html_title:           "Elixir: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Ler arquivos de texto é uma tarefa comum para programadores em qualquer linguagem de programação. Saber como fazer isso em Elixir pode facilitar ainda mais a manipulação e processamento de dados em seus projetos.

## Como Fazer

A leitura de arquivos de texto em Elixir é uma tarefa simples que pode ser realizada usando as funções `File.stream!` e `Enum.each`.

Primeiro, vamos criar um arquivo de texto com o nome "exemplo.txt" na mesma pasta do seu código Elixir. Este arquivo pode conter qualquer texto que você queira ler.

Agora, vamos escrever um código Elixir para ler o conteúdo do arquivo e imprimir na tela:

```Elixir
File.stream!("exemplo.txt") |>
Enum.each(&IO.puts/1)
```

A primeira linha abre o arquivo "exemplo.txt" e cria um fluxo de dados que contém o conteúdo do arquivo. Em seguida, usamos a função `Enum.each` para iterar sobre cada linha do arquivo e imprimi-la usando a função `IO.puts`.

Se você executar esse código, verá o conteúdo do arquivo impresso no seu terminal.

## Deep Dive

Existem outras maneiras de ler arquivos de texto em Elixir, mas a abordagem acima é a mais simples e eficiente. Além disso, usando a função `File.stream!` ao invés de `File.read!` é uma escolha melhor quando se trabalha com arquivos grandes, pois permite que o conteúdo seja lido de forma assíncrona e em blocos menores.

Você também pode especificar encoding e opções de leitura, como pular linhas em branco, ao utilizar `File.stream!` para personalizar sua leitura de arquivo.

Para mais informações sobre leitura de arquivos em Elixir, consulte a documentação oficial: [https://hexdocs.pm/elixir/File.html#stream!/2](https://hexdocs.pm/elixir/File.html#stream!/2)

## Veja Também

- [Como escrever em arquivos de texto em Elixir](https://example.com)
- [Documentação oficial sobre leitura de arquivos](https://hexdocs.pm/elixir/File.html#stream!/2)
- [Outros exemplos de leitura de arquivos em Elixir](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/test/elixir_file_test.exs)