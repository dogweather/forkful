---
title:                "Trabalhando com YAML"
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

YAML, que significa "YAML Ain't Markup Language", é um formato de serialização de dados legível para humanos, usado frequentemente para configurações de projeto e transmissão de dados entre linguagens. Programadores usam YAML pela sua simplicidade e legibilidade, facilitando a modificação e compreensão de estruturas de dados complexas sem a complexidade de outros formatos como XML.

## Como Fazer:

Para trabalhar com YAML em Ruby, use a gem 'yaml'. Primeiro, instale-a com `gem install yaml`. Depois, veja como carregar um YAML, alterar e salvar:

```Ruby
require 'yaml'

# Carregar YAML de um arquivo
config = YAML.load_file('config.yml')

# Acesso e manipulação de dados
config['setting1'] = 'Novo valor'

# Salvar em YAML
File.open('config.yml', 'w') { |file| file.write(config.to_yaml) }
```

Se o `config.yml` for assim:
```yaml
setting1: valor1
setting2: valor2
```

A saída será o arquivo `config.yml` atualizado:
```yaml
---
setting1: Novo valor
setting2: valor2
```

## Mergulho Profundo:

YAML foi introduzido em 2001, projetado para ser mais legível e simples que o XML. Várias linguagens têm bibliotecas para trabalhar com YAML, como PyYAML para Python e go-yaml para Go. Em Ruby, a biblioteca padrão Psych é a implementação mais comum e está incluso por padrão desde a versão 1.9.3. Psych é baseado na libyaml, uma biblioteca em C para parsing e emissão de YAML, garantindo rapidez e eficiência no processamento dos dados.

## Veja Também:

- Documentação oficial do Psych em Ruby: https://ruby-doc.org/stdlib/libdoc/psych/rdoc/Psych.html
- YAML: http://yaml.org/
- Tutorial de YAML com exemplos: https://learnxinyminutes.com/docs/yaml/
- Ruby Gems para trabalhar com YAML: https://rubygems.org/gems/yaml
- Especificação YAML: https://yaml.org/spec/1.2/spec.html