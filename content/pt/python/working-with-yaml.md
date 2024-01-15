---
title:                "Trabalhando com yaml"
html_title:           "Python: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML é uma linguagem de serialização de dados que pode ser usada para estruturar e armazenar informações. Com sua sintaxe simples e fácil de ler, é uma ótima opção para aqueles que precisam organizar grandes quantidades de dados de forma eficiente.

## Como usar YAML em Python

Para trabalhar com YAML em Python, é necessário importar a biblioteca `pyyaml` e usar a função `yaml.safe_load()` para carregar um arquivo YAML em formato de dicionário. Aqui está um exemplo:

```python
import yaml

with open('dados.yml', 'r') as arquivo:
    dados = yaml.safe_load(arquivo)

print(dados)
```

Neste exemplo, usamos a função `open()` para abrir um arquivo YAML chamado "dados.yml" no modo de leitura. Em seguida, usamos a função `yaml.safe_load()` para carregar os dados do arquivo em um dicionário chamado "dados". Por fim, imprimimos o dicionário para confirmar que os dados foram carregados corretamente.

## Aprofundando-se em YAML

YAML é um formato bastante versátil e possui muitos recursos que podem ser explorados. Por exemplo, ele suporta diferentes tipos de dados, como strings, números, listas e dicionários. Além disso, é possível criar referências e incluir outros arquivos YAML dentro de um arquivo principal.

Outro recurso interessante do YAML é a capacidade de adicionar comentários, o que pode ser útil para explicar partes do código ou fazer anotações. Os comentários começam com o caractere `#` e são ignorados pelo interpretador.

Para saber mais sobre YAML e suas funcionalidades, consulte a documentação oficial do pyyaml ou outros recursos online.

## Veja também

- [Documentação oficial do pyyaml](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Tutorial de YAML em Python](https://www.datacamp.com/community/tutorials/yaml-python)
- [Conheça mais sobre a linguagem de serialização YAML](https://www.mundojs.com.br/2017/06/05/conhecendo-yaml/)