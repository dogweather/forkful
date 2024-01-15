---
title:                "Trabalhando com yaml"
html_title:           "Bash: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML, ou YAML Ain't Markup Language, é uma linguagem de serialização de dados leve e fácil de ler. Ela é comumente usada para representar configurações e dados estruturados em ferramentas de automação como Ansible, Kubernetes e Docker. Ao aprender como trabalhar com YAML, você poderá criar e manipular arquivos de configuração de forma mais eficiente e simplificar seus processos de automação.

## Como fazer isso

Para começar a trabalhar com YAML, você precisará ter o Bash instalado em seu sistema. Em seguida, crie um arquivo YAML com a extensão `.yml` ou `.yaml` e adicione seu conteúdo usando uma sintaxe de chave-valor. Veja um exemplo simples abaixo:

```Bash
# arquivo.yml
nome: João
idade: 25
cidade: São Paulo
```

Você pode adicionar quantas chaves e valores desejar, desde que siga a mesma formatação. Agora, para ler os dados deste arquivo YAML, podemos usar o comando `cat`:

```Bash
cat arquivo.yml
```

A saída será:

```Bash
nome: João
idade: 25
cidade: São Paulo
```

Para adicionar comentários em um arquivo YAML, use o caractere `#` antes da linha de comentário. Para criar uma lista, basta adicionar um hífen antes de cada item. Veja um exemplo abaixo:

```Bash
# arquivo.yml
 #Este é um comentário
pessoas:
- João #Primeiro nome
- Maria #Segundo nome
- Pedro #Terceiro nome
```

Agora, para acessar cada item da lista, podemos usar a ferramenta de linha de comandos `jq`:

```Bash
jq '.pessoas[]' arquivo.yml
```

A saída será:

```Bash
"João"
"Maria"
"Pedro"
```

## Mergulho profundo

O YAML é muito flexível e possui várias funcionalidades para ajudá-lo em suas tarefas de automação. Você pode usar as chaves `include` e `extends` para reduzir a repetição de código em seus arquivos YAML e torná-los mais legíveis. Além disso, o YAML suporta tipos de dados complexos, como listas de listas e dicionários, que podem ser úteis em casos de configurações mais complexas.

Outra funcionalidade interessante é a capacidade de referenciar variáveis em outros arquivos YAML, o que pode ser útil ao trabalhar com vários arquivos de configuração interdependentes.

Para obter uma lista completa de recursos e sintaxe do YAML, consulte a documentação oficial disponível em [yaml.org](https://yaml.org).

## Veja também

- [Bash Documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [YAML Quick Reference](https://yaml.org/refcard.html)
- [jq Documentation](https://stedolan.github.io/jq/manual/)