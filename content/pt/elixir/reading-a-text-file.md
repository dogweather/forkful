---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um arquivo de texto com Elixir

## O que e Por quê?

Ler um arquivo de texto é um processo de extração de dados de um arquivo em disco para a memória. Programadores fazem isso para manipular dados e informações que são armazenada em arquivos.

## Como fazer:

Elixir tem funções integradas para manipular arquivos. Abaixo temos um exemplo de como ler um arquivo de texto usando Elixir.

```elixir
{:ok, texto} = File.read("path_do_arquivo.txt")
IO.puts texto
```
Assumindo que `path_do_arquivo.txt` contém "Olá, Elixir!", a saída será:

```
Olá, Elixir!
```
## Mergulho Profundo

Historicamente, a leitura de arquivos foi um aspecto crucial dos sistemas operacionais desde o início dos computadores. Elixir, sendo uma linguagem funcional moderna, facilita bastante essa tarefa.

Como alternativa, você pode usar `File.stream!` para ler o arquivo de texto linha por linha, o que pode ser mais eficiente para arquivos grandes:

```elixir
 File.stream!("path_do_arquivo.txt")
 |> Enum.each(&IO.puts/1)
```
A função `File.read` usa a implementação do Erlang para ler arquivos - é por isso que retorna uma tupla: o primeiro elemento é um átomo indicando sucesso ou falha, e o segundo elemento é o resultado real da operação.

## Ver também

Você pode achar útil olhar a documentação oficial sobre módulos de arquivo e IO em Elixir [aqui](https://hexdocs.pm/elixir/File.html) e [aqui](https://hexdocs.pm/elixir/IO.html).

No geral, a leitura de arquivos de texto em Elixir é direta e leva vantagem da clareza e concisão da sintaxe da linguagem.