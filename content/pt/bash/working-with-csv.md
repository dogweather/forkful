---
title:                "Trabalhando com csv"
html_title:           "Bash: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é isso e por que os programadores o usam?
CSV (Comma Separated Values) é um formato de arquivo usado para armazenar dados tabulares, como planilhas. Ele é composto por colunas e linhas, com cada coluna sendo separada por vírgulas. Os programadores trabalham com arquivos CSV para importar e exportar dados de diferentes fontes, como bancos de dados e planilhas, facilitando o compartilhamento e análise de informações.

## Como fazer:
Exemplo de código:
```
Bash
# Importando dados de um arquivo CSV para uma variável
dados=$(cat arquivo.csv)

# Separando os dados em colunas e extraindo informações específicas
echo "$dados" | cut -d ',' -f 2,3 # seleciona colunas 2 e 3
echo "$dados" | grep "nome" # procura pela palavra "nome" nos dados
```

Exemplo de saída:
```
exemplo1, exemplo2
nome: exemplo
```

## Aprofundando:
CSV foi criado na década de 1970 para facilitar a troca de dados entre programas diferentes. Alguns formatos de arquivos alternativos incluem XML e JSON, mas o CSV é mais simples e amplamente utilizado devido à sua estrutura básica. Para trabalhar com arquivos CSV em Bash, é necessário estar familiarizado com os comandos de manipulação de texto, como cut e grep.

## Veja também:
- [CSV no Wikipedia](https://pt.wikipedia.org/wiki/Comma-separated_values)