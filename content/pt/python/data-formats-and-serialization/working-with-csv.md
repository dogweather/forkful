---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:56.731124-07:00
description: "Trabalhar com CSV (Valores Separados por V\xEDrgula) envolve ler e escrever\
  \ dados em arquivos CSV, um formato comum para armazenamento de dados tabulares.\u2026"
lastmod: 2024-02-19 22:05:05.247715
model: gpt-4-0125-preview
summary: "Trabalhar com CSV (Valores Separados por V\xEDrgula) envolve ler e escrever\
  \ dados em arquivos CSV, um formato comum para armazenamento de dados tabulares.\u2026"
title: Trabalhando com CSV
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Trabalhar com CSV (Valores Separados por Vírgula) envolve ler e escrever dados em arquivos CSV, um formato comum para armazenamento de dados tabulares. Programadores fazem isso para trocar e armazenar dados de maneira fácil num formato baseado em texto simples, amplamente suportado em diferentes plataformas e linguagens.

## Como fazer:
Python fornece o módulo integrado `csv` para manipular arquivos CSV, tornando simples ler e escrever neles. Para manipulação de dados mais robusta e complexa, a biblioteca de terceiros `pandas` é muito popular.

### Usando o módulo `csv`

#### Lendo um arquivo CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    leitor_csv = csv.reader(file)
    for row in leitor_csv:
        print(row)
```
*Assumindo que `sample.csv` contém:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Saída:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Escrevendo em um arquivo CSV
```python
import csv

linhas = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    escritor = csv.writer(file)
    escritor.writerows(linhas)
```
*Cria ou sobrescreve `output.csv` com:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Usando `pandas` para CSV
`pandas` é uma biblioteca poderosa para manipulação de dados que simplifica trabalhar com arquivos CSV entre outros formatos de dados.

#### Instalar pandas
```shell
pip install pandas
```

#### Lendo um arquivo CSV com pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Saída:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Escrevendo em um arquivo CSV com pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Cria ou sobrescreve `output_pandas.csv` com:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
