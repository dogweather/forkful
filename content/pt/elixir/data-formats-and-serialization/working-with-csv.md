---
title:                "Trabalhando com CSV"
date:                  2024-02-03T19:19:30.821288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com arquivos CSV (Valores Separados por Vírgula) envolve ler e escrever dados nesses arquivos, uma necessidade comum para tarefas que requerem importação/exportação de dados ou soluções simples de armazenamento. Programadores aproveitam essa funcionalidade para intercâmbio de dados entre sistemas, edição rápida de dados ou para situações onde um formato de dados leve e facilmente manipulável é vantajoso.

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
