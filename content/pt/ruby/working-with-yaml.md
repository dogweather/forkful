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

## Por que
Há muitas razões pelas quais alguém pode querer trabalhar com YAML em Ruby. YAML é uma linguagem simples e intuitiva para armazenar e transmitir dados estruturados, tornando-a uma escolha popular para configurações de aplicativos e comunicação entre sistemas.

## Como Fazer
Para começar a trabalhar com YAML em Ruby, primeiro precisamos instalar a biblioteca "yaml" usando o gerenciador de pacotes RubyGems. Em seguida, podemos importar a biblioteca em nosso código usando a função "require".

```Ruby
require 'yaml'

# Criando um objeto YAML
dados = { nome: "João", sobrenome: "Silva", idade: 30 }

# Convertendo para YAML
yaml_dados = dados.to_yaml

# Imprimindo o conteúdo YAML
puts yaml_dados

# Saída:
# --- # A linha três é apenas um comentário de organização
# :nome: João
# :sobrenome: Silva
# :idade: 30
```

Para carregar dados YAML em nosso código, podemos usar o método "load_file" da biblioteca YAML. Precisamos fornecer o caminho do arquivo YAML que desejamos carregar e, em seguida, podemos acessar os dados como se fosse uma hash.

```Ruby
require 'yaml'

# Carregando dados YAML
dados = YAML.load_file("dados.yml")

# Acessando dados
puts dados[:nome]

# Saída:
# João
```

## Mergulho Profundo
Além de armazenar e transmitir dados estruturados, YAML também pode ser usado para criar configurações para aplicativos Ruby ou até mesmo para armazenar dados persistentes em um arquivo. Se você quiser se aprofundar mais em YAML, você pode conferir o guia de referência da linguagem oficial [aqui](https://yaml.org/spec/1.2/spec.html).

## Veja também
- [Documentação oficial da biblioteca YAML em Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)
- [Guia de referência da linguagem YAML](https://yaml.org/spec/1.2/spec.html)