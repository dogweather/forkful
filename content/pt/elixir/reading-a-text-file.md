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

## O que & Porquê?
Ler um arquivo de texto é quando o programador deseja acessar o conteúdo de um arquivo de texto e manipulá-lo de alguma forma. Isso pode ser necessário para armazenar informações ou fazer cálculos com base nos dados contidos no arquivo.

## Como fazer:
Usando a linguagem de programação Elixir, podemos facilmente ler um arquivo de texto usando as funções `File.read` e `IO.read`. Primeiro, precisamos abrir o arquivo usando `File.open` e especificar o modo de leitura. Então, podemos usar `IO.read` para ler uma linha do arquivo por vez. Um exemplo de código seria assim:

```elixir
File.open("exemplo.txt", [:read]) do |file|
  IO.read(file)
end

# Output:
# "Linha 1"
# "Linha 2"
# "Linha 3"
```

## Mergulho Profundo:
Ler arquivos de texto é uma tarefa básica e necessária em muitos tipos de aplicativos. Antes de aprendermos a fazê-lo em Elixir, é importante entendermos a importância da leitura de arquivos na programação. Além disso, existem outras formas de ler arquivos, como usar bibliotecas externas ou armazená-los em bancos de dados.

## Veja também:
Para mais informações sobre a leitura de arquivos em Elixir, consulte a documentação oficial em [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html).