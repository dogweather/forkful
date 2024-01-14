---
title:                "Fish Shell: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

#
## Por que trabalhar com CSV usando o Fish Shell?

Se você está lidando com dados em formato CSV, o Fish Shell pode ser uma excelente ferramenta para agilizar seu trabalho. Com seus recursos de processamento de texto e gerenciamento de arquivos, o Fish Shell possibilita uma maneira simplificada e eficiente de lidar com arquivos CSV.

## Como fazer:

### Lendo um arquivo CSV:

Para ler um arquivo CSV usando o Fish Shell, podemos usar o comando `read` seguido pelo nome do arquivo. Por exemplo:

```Fish Shell
read -C arquivo.csv
```

Isso irá imprimir o conteúdo do arquivo CSV na tela, separado por vírgulas. Para salvar esse conteúdo em uma variável, podemos usar o sinal de atribuição `=`, como mostrado abaixo:

```Fish Shell
conteudo = (read -C arquivo.csv)
```

### Manipulando dados CSV:

Com os dados CSV armazenados em uma variável, podemos usar os recursos de processamento de texto do Fish Shell para manipulá-los da maneira desejada. Por exemplo, podemos usar o comando `grep` para filtrar dados específicos com base em um critério. No exemplo abaixo, apenas as linhas que contêm a palavra "venda" serão impressas:

```Fish Shell
echo $conteudo | grep venda
```

Também é possível usar o comando `awk` para realizar operações e cálculos em colunas específicas de um arquivo CSV. No exemplo abaixo, a terceira coluna será multiplicada por 2 e o resultado será impresso na tela:

```Fish Shell
echo $conteudo | awk -F ',' '{print $3*2}'
```

## Mergulho profundo:

Existem várias vantagens em usar o Fish Shell para trabalhar com arquivos CSV. Uma delas é o seu recurso de expansão de comandos, que permite que você execute um comando em todos os arquivos de um diretório de uma só vez. Por exemplo, se você tiver vários arquivos CSV em um diretório, pode usar a seguinte sintaxe para executar um comando em todos eles:

```Fish Shell
comando **.csv
```

Outro recurso útil do Fish Shell é a capacidade de trabalhar com dados semelhantes a tabelas usando a ferramenta `tabulate`. Essa ferramenta cria uma tabela com base nos dados fornecidos, tornando mais fácil visualizar e analisar informações de um arquivo CSV.

## Veja também:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Guia rápido para iniciantes do Fish Shell](https://medium.com/@cnissen23/a-beginners-guide-to-fish-shell-f772c377cfc9)
- [Tutorial de processamento de texto no Fish Shell](https://spin.atomicobject.com/2018/01/22/text-processing-fish-shell/)