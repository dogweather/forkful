---
title:                "Trabalhando com yaml"
html_title:           "Gleam: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Se você é um programador em busca de uma linguagem de configuração flexível e amigável, o YAML pode ser a solução perfeita para você. Com sua sintaxe simples e facilidade de leitura, você pode criar e personalizar facilmente seus arquivos de configuração, tornando o processo de desenvolvimento muito mais eficiente.

## Como fazer isso?

Para começar a trabalhar com YAML em seu código Gleam, basta importar a biblioteca "yaml" e criar um arquivo YAML usando o módulo "Yaml.encode". Você pode então acessar os dados do arquivo YAML usando o módulo "Yaml.decode". Veja um exemplo abaixo:

```
Gleam import yaml

yaml_data = yaml.encode({"nome": "Ana", "idade": 25, "hobby": "cozinhar"})

decoded_data = yaml.decode(yaml_data)

the_name = decoded_data["nome"]
the_hobby = decoded_data["hobby"]

IO.println("Meu nome é $(the_name) e meu hobby é $(the_hobby).")
```

A saída seria:

```
Meu nome é Ana e meu hobby é cozinhar.
```

## Aprofundando-se em YAML

Além de sua sintaxe simples e legível, YAML também possui recursos poderosos que facilitam o trabalho com configurações complexas. Você pode criar variáveis e referenciar valores em diferentes partes do arquivo YAML, tornando-o facilmente personalizável.

Além disso, você também pode incluir comentários em seu arquivo YAML para facilitar a compreensão e organização do código. Basta adicionar o símbolo "#" antes da linha do comentário.

Outra vantagem do YAML é sua compatibilidade com outras linguagens de programação, o que permite que você compartilhe e utilize seus arquivos de configuração em diferentes projetos.

## Veja também

- [Documentação do YAML](https://yaml.org/spec/1.2/spec.html)
- [Guia de referência YAML para iniciantes](https://codebeautify.org/yaml-editor)
- [Exemplos de uso do YAML em Gleam](https://github.com/gleam-lang/gleam/blob/master/tests/yaml_test.gleam)