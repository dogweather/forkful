---
title:                "Trabalhando com yaml"
html_title:           "Ruby: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Trabalhar com YAML é uma maneira simples de armazenar e transmitir dados estruturados em um formato legível para humanos. Programadores usam YAML para configurar e armazenar dados de forma mais eficiente do que em outros formatos, como JSON ou XML.

## Como fazer:

Para utilizar YAML em seu código Ruby, primeiro você precisa requerer o módulo YAML. Em seguida, é possível carregar e manipular dados YAML usando os métodos fornecidos pelo módulo, como `YAML.load` e `YAML.dump`.

Exemplo de código:

```Ruby
require 'yaml'

# carregar dados YAML de um arquivo
data = YAML.load(File.read('arquivo.yaml'))

# modificar os dados e salvá-los em um novo arquivo
data['chave'] = valor
File.write('novo_arquivo.yaml', YAML.dump(data))
```

Exemplo de saída:

```Ruby
chave: valor
```

## Profundando:

YAML, que é a sigla para "YAML Ain't Markup Language", é um formato de serialização inventado em 2001 por um programador que queria uma maneira mais intuitiva de armazenar dados estruturados do que XML. Outras alternativas ao YAML incluem JSON e XML, mas YAML é frequentemente escolhido devido à sua simplicidade e legibilidade. É importante lembrar que YAML é um formato que não permite a execução de código, tornando-o mais seguro do que outros formatos que permitem a injeção de comandos.

## Veja também:

- Documentação oficial do módulo YAML para Ruby: https://ruby-doc.org/stdlib-2.7.0/libdoc/yaml/rdoc/YAML.html
- Tutorial de YAML para iniciantes: https://www.tutorialspoint.com/ruby/yaml_really_ain_t_markup_language.htm
- Comparação entre YAML, JSON e XML: https://medium.com/swlh/yaml-vs-json-vs-xml-which-one-to-choose-and-why-1e4281b53b9b