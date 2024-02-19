---
aliases:
- /pt/bash/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:40.153847-07:00
description: "Trabalhar com arquivos CSV (Valores Separados por V\xEDrgula) no Bash\
  \ \xE9 sobre processar e manipular dados tabulares armazenados em formato de texto\
  \ simples.\u2026"
lastmod: 2024-02-18 23:08:58.347489
model: gpt-4-0125-preview
summary: "Trabalhar com arquivos CSV (Valores Separados por V\xEDrgula) no Bash \xE9\
  \ sobre processar e manipular dados tabulares armazenados em formato de texto simples.\u2026"
title: Trabalhando com CSV
---

{{< edit_this_page >}}

## O que é & Por que?
Trabalhar com arquivos CSV (Valores Separados por Vírgula) no Bash é sobre processar e manipular dados tabulares armazenados em formato de texto simples. Isso é essencial para programadores, pois permite a automação de tarefas de transformação, análise e integração de dados diretamente da linha de comando, sem a necessidade de ferramentas mais pesadas ou ambientes de programação.

## Como fazer:

**Lendo um Arquivo CSV Linha por Linha**

```bash
while IFS=, read -r coluna1 coluna2 coluna3
do
  echo "Coluna 1: $coluna1, Coluna 2: $coluna2, Coluna 3: $coluna3"
done < exemplo.csv
```

*Saída de exemplo:*

```
Coluna 1: id, Coluna 2: nome, Coluna 3: email
...
```

**Filtrando Linhas do CSV com Base em uma Condição**

Usando `awk`, você pode facilmente filtrar linhas. Por exemplo, para encontrar linhas onde a segunda coluna é igual a "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' exemplo.csv
```

**Modificando o Valor de uma Coluna**

Para alterar a segunda coluna para maiúsculas:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' exemplo.csv
```

**Ordenando um Arquivo CSV Baseado em uma Coluna**

Você pode ordenar um arquivo CSV baseado, digamos, na terceira coluna (numericamente):

```bash
sort -t, -k3,3n exemplo.csv
```

**Usando `csvkit` para Tarefas Mais Complexas**

`csvkit` é um conjunto de ferramentas de linha de comando para converter para e trabalhar com CSV. Pode ser instalado via pip.

Para converter um arquivo JSON para CSV:

```bash
in2csv data.json > data.csv
```

Para consultar um arquivo CSV usando SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" exemplo.csv
```

*Nota: A instalação do `csvkit` requer Python e pode ser feita usando `pip install csvkit`.*
