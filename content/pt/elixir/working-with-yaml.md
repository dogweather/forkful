---
title:                "Elixir: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML no Elixir?

YAML é uma linguagem de serialização de dados que é amplamente usada para configurações e arquivos de dados estruturados. No contexto de programação Elixir, manipular dados em formato YAML pode ser muito útil para tarefas como leitura e escrita de configurações de aplicativos, geração de relatórios ou até mesmo para comunicação com outras linguagens de programação.

## Como fazer:

Para trabalhar com YAML no Elixir, primeiramente é necessário instalar a biblioteca `YamlElixir`. Isso pode ser feito através do gerenciador de pacotes `mix`, seguindo os passos abaixo:

1. Abra o terminal e navegue até o diretório do seu projeto Elixir.
2. Execute o comando `mix deps.get` para baixar as dependências do projeto.
3. Adicione a biblioteca `YamlElixir` ao seu arquivo `mix.exs` na seção `deps`:

    ```Elixir
    defp deps do
      [
        {:yaml_elixir, "~> 0.0.1"}
      ]
    end
    ```
4. Execute o comando `mix deps.compile` para compilar a biblioteca.
5. Agora você pode importar o módulo `YamlElixir` e começar a trabalhar com YAML em seu código Elixir.

### Lendo um arquivo YAML:

```Elixir
# Importando o módulo
import YamlElixir
# Lendo o arquivo
{:ok, data} = read_file("config.yml")
```

### Escrevendo em um arquivo YAML:

```Elixir
# Criando um mapa com os dados
data = %{nome: "Elixir", categoria: "linguagem"}
# Escrevendo no arquivo
{:ok, _} = write_file("data.yml", data)
```

### Convertendo dados para YAML:

```Elixir
# Criando uma lista
lista = ["Elixir", "é", "incrível"]
# Convertendo para YAML
yaml = to_yaml(lista)
```

## Mergulho Profundo:

Por baixo dos panos, a biblioteca `YamlElixir` utiliza a biblioteca `LibYAML` escrita em C, que lida com a análise sintática e serialização de YAML. Por isso, é importante seguir as convenções de formatação de YAML para garantir que o código funcione corretamente.

Além disso, a biblioteca `YamlElixir` oferece funções adicionais para manipulação de dados YAML, como leitura e escrita a partir de strings e conversão para formatos de dados como JSON.

## Veja também:

- [Documentação da biblioteca YamlElixir](https://hexdocs.pm/yaml_elixir/api-reference.html)
- [Documentação oficial do YAML](https://yaml.org/)
- [Site oficial do Elixir](https://elixir-lang.org/)