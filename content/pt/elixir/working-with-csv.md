---
title:                "Elixir: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que Trabalhar com CSV em Elixir?

Trabalhar com CSV (Comma Separated Values) é uma tarefa comum em muitos projetos de programação. Elixir oferece ótimas ferramentas para lidar com essa tarefa, tornando mais fácil e eficiente trabalhar com esse tipo de arquivo. Além disso, trabalhar com CSV pode ser útil para importar e exportar dados de bancos de dados, gerar relatórios e muito mais.

## Como Fazer:

Para começar, é necessário instalar a biblioteca CSV de Elixir. Você pode fazer isso através do gerenciador de pacotes Hex digitando o seguinte comando no terminal: 

```Elixir
mix hex.install csv
```

Em seguida, você precisa importar a biblioteca no seu arquivo Elixir com o seguinte código: 

```Elixir
import CSV
```

Agora, vamos criar um arquivo CSV básico com algumas informações. Utilizaremos o comando `CSV.encode/2` para criar o arquivo e inseriremos algumas linhas de dados. Veja o exemplo abaixo:

```Elixir
CSV.encode("arquivo.csv", [
  ["Nome", "Idade", "Profissão"],
  ["João", 30, "Desenvolvedor"],
  ["Maria", 28, "Designer"],
  ["Pedro", 35, "Gerente"]
])
```

Esse código criará um arquivo chamado "arquivo.csv" no diretório do seu projeto com os dados especificados. Se você abrir esse arquivo, poderá ver que ele está estruturado em linhas e colunas.

Para ler e manipular um arquivo CSV existente, podemos utilizar o comando `CSV.decode/2`. Veja o exemplo abaixo:

```Elixir
CSV.decode("arquivo.csv")
|> Enum.each(fn row ->
  nome = row["Nome"]
  idade = row["Idade"]
  profissão = row["Profissão"]
  IO.puts nome <> " tem " <> idade <> " anos e é " <> profissão
end)
```

Esse código lerá o arquivo CSV criado anteriormente e imprimirá as informações de cada linha formatadas em uma frase.

## Aprofundando Mais:

Existem outras opções e funções que a biblioteca CSV de Elixir oferece para trabalhar com esses tipos de arquivos. É possível definir o delimitador dos dados, separar linhas de cabeçalho, tratar valores vazios e até mesmo criar funções customizadas para manipular os dados. É possível encontrar mais informações sobre essas funcionalidades na documentação oficial do CSV para Elixir.

Outra ferramenta útil é a biblioteca Elixir-CSV, que permite a importação de arquivos CSV diretamente para bancos de dados PostgreSQL sem a necessidade de manipulação de linhas e colunas.

## Veja Também:

- [Documentação Oficial do CSV para Elixir](https://hexdocs.pm/csv/CSV.html)
- [GitHub da Biblioteca Elixir-CSV](https://github.com/beatrichartz/csv-elixir)