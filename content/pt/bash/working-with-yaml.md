---
title:                "Bash: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Se você é um programador iniciante ou experiente, provavelmente já se deparou com o formato YAML em algum momento. YAML é uma linguagem de marcação muito flexível e fácil de ler, que é usada para estruturar dados em diferentes aplicações. É uma ótima opção para formatar e compartilhar informações em um formato legível para humanos e fácil de entender para as máquinas.

## Como trabalhar com YAML?

Aqui, vamos explorar alguns conceitos básicos de programação Bash para trabalhar com YAML. Primeiro, é importante entender que YAML é baseado em pares de chaves e valores. Ou seja, os dados são organizados em uma estrutura de "chave: valor". Abaixo está um exemplo simples de um arquivo YAML:

```Bash
nome: João
sobrenome: Silva
idade: 30
```

Para acessar esses dados em um script Bash, usamos a ferramenta "yq", que pode ser instalada através do gerenciador de pacotes "apt" no Ubuntu:

```Bash
sudo apt install yq
```

Em seguida, usamos o seguinte código para imprimir o valor da chave "nome":

```Bash
valor=$(yq r arquivo.yaml nome)
echo $valor
```

O resultado será "João". É importante notar que o yq irá retornar todos os valores das chaves se houver mais de um no arquivo YAML.

## Mergulhando mais fundo

Além de apenas acessar os dados de um arquivo YAML, também é possível modificar e criar novos arquivos usando comandos Bash. Por exemplo, podemos adicionar um novo par de chave e valor ao nosso arquivo existente usando o seguinte código:

```Bash
yq w -i arquivo.yaml cidade "São Paulo"
```

Isso adicionará uma nova linha ao nosso arquivo YAML:

```
cidade: São Paulo
```

Para aqueles que desejam se aprofundar ainda mais, existem muitas ferramentas e bibliotecas disponíveis para trabalhar com YAML em diferentes plataformas e linguagens de programação. É sempre útil pesquisar e aprender mais sobre essas ferramentas para tornar o processo mais eficiente e fácil.

## Veja também

- [Documentação do YAML](https://yaml.org/)
- [Repositório GitHub do yq](https://github.com/mikefarah/yq)
- [Diferença entre YAML e JSON](https://www.educba.com/yaml-vs-json/)