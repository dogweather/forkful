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

## Por que escrever um arquivo de texto em Elixir?

Escrever um arquivo de texto é útil em muitos cenários, como gerar relatórios, salvar dados importantes ou armazenar informações para uso posterior. Aprender a fazê-lo em Elixir pode ajudar a expandir suas habilidades de programação e a lidar com tarefas comuns de forma eficiente.

## Como Fazer

Para escrever um arquivo de texto em Elixir, primeiro precisamos abrir um arquivo usando a função `File.open/2` e passar o nome do arquivo e o modo de abertura como argumentos:

```
iex> {:ok, file} = File.open("meu_arquivo.txt", [:write])
{:ok, #PID<0.86.0>}
```

Agora podemos usar a função `IO.write/2` para escrever o conteúdo no arquivo aberto anteriormente:

```
iex> IO.write(file, "Este é um exemplo de texto que será escrito em um arquivo.")
:ok
```

Em seguida, devemos fechar o arquivo usando a função `File.close/1` para salvar as alterações:

```
iex> File.close(file)
:ok
```

Podemos verificar se o arquivo foi criado e se o conteúdo foi escrito corretamente usando a função `File.read/1`:

```
iex> File.read("meu_arquivo.txt")
{:ok, "Este é um exemplo de texto que será escrito em um arquivo."}
```

## Deep Dive

Ao escrever um arquivo de texto em Elixir, é importante entender o conceito de manipuladores. Manipuladores são funções que são chamadas quando os dados são gravados no arquivo. Eles são passados como argumentos para a função `File.open/2` e podem ser usados para controlar o modo como os dados são gravados no arquivo.

Existem três manipuladores principais que podemos usar ao escrever um arquivo de texto em Elixir: `:raw`, `:line` e `:unicode`. O manipulador `:raw` escreve dados em formato de sequência de bytes, o manipulador `:line` adiciona uma quebra de linha ao final de cada bloco de dados e o manipulador `:unicode` codifica os dados em formato Unicode. É possível usar qualquer um desses manipuladores ou uma combinação deles, dependendo do tipo de dados que estamos escrevendo.

## Veja Também

- [Guia oficial sobre I/O em Elixir](https://hexdocs.pm/elixir/master/IO.html)
- [Documentação sobre a função File.open/2](https://hexdocs.pm/elixir/File.html#open/2)
- [Tutorial sobre a manipulação de arquivos em Elixir](https://blog.appsignal.com/2019/08/13/handling-files-in-elixir.html)