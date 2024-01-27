---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com CSV (Valores Separados por Vírgula) envolve ler, escrever e manipular dados em um formato amplamente utilizado para troca de dados estruturados. Programadores recorrem a CSV pela simplicidade e interoperabilidade entre sistemas e linguagens de programação.

## Como Fazer:
```Fish Shell
# Lendo um arquivo CSV linha por linha
for line in (cat dados.csv)
    echo $line
end

# Dividindo os campos e acessando o segundo valor de cada linha
for line in (cat dados.csv)
    set -l campos (string split "," $line)
    echo $campos[2]
end

# Convertendo um arquivo CSV em um array de arrays (linhas e campos)
set -l linhas_e_campos
for line in (cat dados.csv)
    set -l campos (string split "," $line)
    set linhas_e_campos $linhas_e_campos $campos
end
```

## Mergulho Profundo:
O CSV é um formato de arquivo que data dos primeiros dias da computação, servindo como uma forma simples de importar e exportar dados de tabelas. Alternativas modernas incluem JSON e XML, que oferecem mais complexidade e estrutura. Em Fish Shell, diferentemente de outras shell scripts como Bash, não se usa ferramentas externas como `awk`, portanto a manipulação de CSV é feita através dos comandos internos `string split` e `for`.

## Veja Também:
- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Guia para manipulação de texto no Fish Shell: https://fishshell.com/docs/current/cmds/string.html
- W3C sobre CSV: https://www.w3.org/TR/tabular-data-primer/
