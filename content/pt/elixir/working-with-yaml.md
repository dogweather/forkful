---
title:                "Trabalhando com yaml"
html_title:           "Elixir: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é YAML e porquê usá-lo?

YAML é uma linguagem de marcação criada para estruturar dados em um formato legível por humanos. Programadores usam YAML para armazenar e transferir informações em seus projetos, visto que é fácil de entender e modificar.

## Como fazer:

```Elixir
# Carregar a biblioteca YAML
require EYAML

# Criar um objeto com dados
data = %{ nome: "Maria", idade: 25, profissao: "Programadora" }

# Converter os dados para YAML
YAML.dump(data)

# Resultado
%{ idade: "25", nome: "Maria", profissao: "Programadora" }
```

## Detalhando mais:

YAML foi criado em 2001 por Clark Evans como uma alternativa ao XML para representação de dados. Hoje, é amplamente usado em projetos de desenvolvimento de software. Além disso, existem outras opções, como JSON e TOML, mas YAML é a escolha popular devido à sua sintaxe simplificada e flexibilidade.

Além de gerar dados, também é possível carregar e manipular arquivos YAML em Elixir, usando a função `YAML.load()`.

## Veja também:

 - Documentação oficial do Elixir sobre YAML: https://hexdocs.pm/elixir/1.12/YAML.html
 - Artigo do blog de José Valim sobre o uso de YAML em Elixir: https://blog.plataformatec.com.br/2018/01/working-with-yaml-in-elixir/
 - Projeto no GitHub de YAML em Elixir: https://github.com/KronicDeth/yaml-elixir