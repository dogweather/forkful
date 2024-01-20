---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Trabalhar com CSV (Valores Separados por Vírgula) é lidar com dados em um formato de texto simples muito usado para troca de informações entre diferentes sistemas. Programadores o utilizam pela simplicidade, interoperabilidade e por ser amplamente suportado.

## Como fazer:
```Bash
# Lendo um arquivo CSV linha por linha
while IFS=, read -r col1 col2 col3
do
  echo "Coluna1: $col1 - Coluna2: $col2 - Coluna3: $col3"
done < exemplo.csv
```
Saída esperada:
```
Coluna1: Valor1 - Coluna2: Valor2 - Coluna3: Valor3
...
```
```Bash
# Extraindo dados com 'cut'
cut -d ',' -f 2 exemplo.csv # Retorna a segunda coluna de cada linha
```

## Mergulho Profundo
O formato CSV surgiu na década de 1970 e se tornou um padrão informal ao longo do tempo devido à sua simplicidade. Alternativas, como o formato JSON ou XML, oferecem estruturas mais ricas, mas são mais complexas. Detalhes de implementação no Bash incluem a manipulação do delimitador de campo (IFS) e o uso de loops para processar cada linha.

## Veja Também
- Manual do Bash: https://www.gnu.org/software/bash/manual/
- Tutorial AWK para processamento de CSV: https://www.gnu.org/software/gawk/manual/gawk.html
- Guia avançado de scripts de shell: https://tldp.org/LDP/abs/html/