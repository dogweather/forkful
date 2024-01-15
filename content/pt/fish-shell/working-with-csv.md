---
title:                "Trabalhando com csv"
html_title:           "Fish Shell: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

Trabalhar com CSV (Comma-Separated Values) pode ser muito útil para organizar e manipular dados de forma eficiente. Com esse formato, é possível armazenar e transferir grandes quantidades de dados de forma simples e compacta, tornando-o uma escolha popular para tarefas como importar e exportar dados de planilhas.

## Como fazer

Para trabalhar com CSV no Fish Shell, é necessário utilizar o comando `csvtool`. Ele é uma ferramenta integrada ao Fish Shell que possibilita a leitura e manipulação de arquivos CSV. Para começar, vamos criar um arquivo CSV simples com o nome "exemplo.csv", que contém os dados de alguns funcionários de uma empresa:

```
Fish Shell(csvtool)
$ echo "Nome, Cargo, Salário" > exemplo.csv
$ echo "João, Gerente, 5000" >> exemplo.csv
$ echo "Maria, Analista, 3000" >> exemplo.csv
$ echo "Pedro, Assistente, 2500" >> exemplo.csv
```

Agora, vamos utilizar o `csvtool` para visualizar o conteúdo desse arquivo:

```
Fish Shell(csvtool)
$ csvtool -t ',' transpose exemplo.csv
Nome,Cargo,Salário
João,Gerente,5000
Maria,Analista,3000
Pedro,Assistente,2500
```

Com o comando `csvtool -t ',' transpose`, nós informamos que o separador no nosso arquivo é a vírgula (`','`), e o `transpose` faz com que as colunas sejam modificadas para linhas, tornando mais fácil a visualização dos dados.

Outros comandos úteis para trabalhar com CSV no Fish Shell incluem `csvtool head`, que mostra somente as primeiras linhas do arquivo, e `csvtool count`, que conta o número de linhas do arquivo.

## Mergulho Profundo

O `csvtool` possui muitas outras opções e funcionalidades, como a possibilidade de selecionar e filtrar linhas específicas do arquivo, renomear colunas, calcular estatísticas e até mesmo fazer operações matemáticas com os dados. Para saber mais detalhes, é só consultar a documentação dessa ferramenta ou utilizar o comando `man csvtool` no terminal.

Para trabalhar com arquivos CSV mais complexos, é possível utilizar outras ferramentas como `awk`, `sed` ou `grep`, que também são integradas ao Fish Shell e podem facilitar o processo de leitura e manipulação desses arquivos.

## Veja também

- [Documentação do csvtool](https://fishshell.com/docs/current/commands.html#csvtool)
- [Guia básico de comandos no Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Como trabalhar com arquivos CSV utilizando awk](https://www.digitalocean.com/community/tutorials/how-to-work-with-csv-data-using-awk-commands)