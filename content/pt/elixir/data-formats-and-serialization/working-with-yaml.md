---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:08.055660-07:00
description: "YAML, abrevia\xE7\xE3o de YAML Ain't Markup Language, \xE9 um padr\xE3\
  o de serializa\xE7\xE3o de dados leg\xEDvel por humanos comumente usado para arquivos\
  \ de configura\xE7\xE3o e\u2026"
lastmod: 2024-02-19 22:05:05.335289
model: gpt-4-0125-preview
summary: "YAML, abrevia\xE7\xE3o de YAML Ain't Markup Language, \xE9 um padr\xE3o\
  \ de serializa\xE7\xE3o de dados leg\xEDvel por humanos comumente usado para arquivos\
  \ de configura\xE7\xE3o e\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O quê e Por quê?

YAML, abreviação de YAML Ain't Markup Language, é um padrão de serialização de dados legível por humanos comumente usado para arquivos de configuração e troca de dados entre linguagens com diferentes estruturas de dados. Programadores o utilizam devido à sua simplicidade e capacidade de representar facilmente dados hierárquicos complexos.

## Como fazer:

Elixir não inclui suporte embutido para YAML. No entanto, você pode usar bibliotecas de terceiros, como `yamerl` ou `yaml_elixir`, para trabalhar com YAML. Aqui, vamos nos concentrar em `yaml_elixir` pela sua facilidade de uso e recursos abrangentes.

Primeiro, adicione `yaml_elixir` às suas dependências em mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Depois, execute `mix deps.get` para buscar a nova dependência.

### Lendo YAML

Dado um simples arquivo YAML, `config.yaml`, que se parece com isto:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Você pode ler este arquivo YAML e convertê-lo para um mapa Elixir assim:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Exemplo de uso
Config.read()
# Saída: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### Escrevendo YAML

Para escrever um mapa de volta para um arquivo YAML:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Exemplo de uso
ConfigWriter.write()
# Isso criará ou sobrescreverá `new_config.yaml` com o conteúdo especificado
```

Note como `yaml_elixir` permite uma tradução direta entre arquivos YAML e estruturas de dados Elixir, tornando-o uma excelente escolha para programadores Elixir que precisam trabalhar com dados YAML.
