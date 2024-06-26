---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:30.821288-07:00
description: "Como Fazer: Elixir, com sua poderosa correspond\xEAncia de padr\xF5\
  es e suporte para encadeamento (pipelining), pode manejar arquivos CSV de forma\
  \ eficiente,\u2026"
lastmod: '2024-03-13T22:44:46.260351-06:00'
model: gpt-4-0125-preview
summary: "Elixir, com sua poderosa correspond\xEAncia de padr\xF5es e suporte para\
  \ encadeamento (pipelining), pode manejar arquivos CSV de forma eficiente, mesmo\
  \ sem bibliotecas de terceiros."
title: Trabalhando com CSV
weight: 37
---

## Como Fazer:
Elixir, com sua poderosa correspondência de padrões e suporte para encadeamento (pipelining), pode manejar arquivos CSV de forma eficiente, mesmo sem bibliotecas de terceiros. Contudo, para necessidades mais avançadas, a biblioteca `nimble_csv` é uma opção rápida e direta.

### Lendo um Arquivo CSV Sem Bibliotecas Externas
Você pode ler e analisar um arquivo CSV usando as funções internas do Elixir:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Exemplo de uso
CSVReader.read_file("data.csv")
# Saída: [["Cabeçalho1", "Cabeçalho2"], ["ValorLinha1", "ValorLinha2"], ["ValorLinha2", "ValorLinha2"]]
```

### Escrevendo em um Arquivo CSV
Similarmente, para escrever dados em um arquivo CSV:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Exemplo de uso
dados = [["Cabeçalho1", "Cabeçalho2"], ["Valor1", "Valor2"], ["Valor3", "Valor4"]]
CSVWriter.write_to_file("saida.csv", dados)
# Cria saida.csv com os dados formatados como CSV
```

### Usando `nimble_csv`
Para manipulações de CSV mais complexas, `nimble_csv` proporciona uma maneira poderosa e flexível de trabalhar com dados CSV. Primeiro, adicione `nimble_csv` às suas dependências em `mix.exs` e execute `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Analisando dados CSV com `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Exemplo de uso
MyCSVParser.parse("data.csv")
# A saída com nimble_csv pode ser personalizada com base na definição, mas geralmente se parece com uma lista de listas ou tuplas, dependendo de como você configurou seu analisador.
```

Escrever dados CSV usando `nimble_csv` requer transformar manualmente seus dados em um formato apropriado e então escrevê-los em um arquivo, muito como o exemplo em Elixir puro, mas aproveitando o `nimble_csv` para gerar linhas CSV formatadas corretamente.

Escolhendo a abordagem apropriada para a complexidade da sua tarefa, você pode manipular arquivos CSV em Elixir com grande flexibilidade e poder.
