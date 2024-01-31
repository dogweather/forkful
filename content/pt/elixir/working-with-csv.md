---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é & Porquê?

Trabalhar com CSV (Valores Separados por Vírgula) é manipular arquivos de texto que guardam dados tabelados. Programadores fazem isso para importar, exportar e manipular dados de forma simples e compatível com diferentes sistemas.

## Como Fazer:

```elixir
defmodule CSVExample do
  # Usando a biblioteca CSV
  def parse_and_save(file_path) do
    file_path
    |> File.stream!()
    |> CSV.decode(separator: ?;)
    |> Enum.each(&process_row/1)
  end

  defp process_row(row) do
    # Manipula cada linha aqui
    IO.inspect(row)
  end
end

# Exemplo de uso:
# CSVExample.parse_and_save("dados.csv")
```
Saída de exemplo:
```
["cabeçalho1", "cabeçalho2", "cabeçalho3"]
["valor1", "valor2", "valor3"]
["valor4", "valor5", "valor6"]
```

## Mergulho Profundo

CSV não é um formato padronizado, o que pode levar a diferenças como delimitadores e codificações de caracteres. Existem alternativas como JSON e XML, mas CSV permanece popular pela sua simplicidade e legibilidade. Em Elixir, manipular CSV geralmente exige uma biblioteca, como o pacote `CSV`, que cuida do parsing e escrita dos dados.

## Veja Também:

- [CSV package documentation](https://hexdocs.pm/csv/readme.html) para uma referência completa de como trabalhar com CSV em Elixir.
- [RFC 4180](https://tools.ietf.org/html/rfc4180), Common Format and MIME Type for Comma-Separated Values (CSV) Files, para entender as convenções formalizadas de CSV.
