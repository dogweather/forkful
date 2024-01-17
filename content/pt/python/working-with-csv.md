---
title:                "Trabalhando com csv"
html_title:           "Python: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-csv.md"
---

{{< edit_this_page >}}

## O que e por que?

CSV (Comma-Separated Values) eh um tipo de arquivo que armazena dados em texto, onde os valores sao separados por virgulas. Programadores frequentemente trabalham com arquivos CSV porque eles sao faceis de ler e manipular.

## Como fazer:

Para ler um arquivo CSV em Python, precisamos importar o modulo `csv`. Em seguida, utilizamos a funcao `reader` para ler o arquivo e iterar sobre ele usando um loop for. Aqui esta um exemplo simples:

```Python
import csv

with open('arquivo.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:
        print(row)
```

Isso ira imprimir cada linha do arquivo CSV como uma lista com os valores separados por virgula. 

Para escrever em um arquivo CSV, usamos a funcao `writer` do modulo `csv`. Aqui esta um exemplo:

```Python
import csv

with open('novo_arquivo.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file, delimiter=',')
    csv_writer.writerow(['valor1', 'valor2', 'valor3'])
```

Isso ira criar um arquivo CSV com uma linha contendo os valores fornecidos, separados por virgulas.

## Profundando:

Arquivos CSV sao frequentemente usados em aplicacoes que requerem grandes quantidades de dados, como bancos de dados e planilhas. Eles se tornaram populares no final da decada de 1980, como um formato padrao para compartilhamento de dados entre aplicacoes diferentes.

Uma alternativa aos arquivos CSV sao os arquivos JSON, que sao mais versateis e podem armazenar valores em um formato hierarquico. No entanto, arquivos CSV ainda sao amplamente usados devido a sua simplicidade e legibilidade.

Ao trabalhar com arquivos CSV, e importante ter em mente que ele e apenas um formato de armazenamento de dados, e nao oferece qualquer nivel de seguranca. Portanto, arquivos CSV nao devem ser usados para armazenar informacoes sensiveis.

## Veja tambem:

- [Documentacao oficial do modulo CSV do Python](https://docs.python.org/3/library/csv.html)
- [Introducao ao analise de dados com arquivos CSV em Python](https://realpython.com/python-csv/)
- [Tutorial sobre manipulacao de arquivos CSV em Python](https://www.dataquest.io/blog/python-read-write-csv/)