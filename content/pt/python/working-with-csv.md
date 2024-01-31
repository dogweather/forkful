---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Trabalhar com CSV significa mexer com arquivos de Valores Separados por Vírgulas, úteis para armazenar dados de forma simples. Programadores usam CSV para importar e exportar dados de/para diversos sistemas, pela facilidade de uso e interoperabilidade.

## Como Fazer:

```Python
import csv

# Escrita de CSV
with open('exemplo.csv', mode='w', newline='') as arquivo:
    escritor = csv.writer(arquivo)
    escritor.writerow(['nome', 'cidade', 'idade'])
    escritor.writerow(['Ana', 'Porto', 28])
    escritor.writerow(['Luís', 'Lisboa', 34])

# Leitura de CSV
with open('exemplo.csv', mode='r') as arquivo:
    leitor = csv.reader(arquivo)
    for linha in leitor:
        print(linha)

# Saída esperada
# ['nome', 'cidade', 'idade']
# ['Ana', 'Porto', 28']
# ['Luís', 'Lisboa', 34']
```

## Mergulho Profundo

CSV foi criado nos anos 70 e suportado inicialmente por programas como o Excel. Alternativas incluem JSON e XML, mais complexos, mas com mais funcionalidades. Detalhes importantes de implementação incluem o uso correto de delimitadores e a manipulação de caracteres especiais, como aspas e quebras de linha, dentro dos campos.

## Veja Também:

- Documentação Python sobre o módulo CSV: [https://docs.python.org/3/library/csv.html](https://docs.python.org/3/library/csv.html)
- Tutorial de CSV em Python na Real Python: [https://realpython.com/python-csv/](https://realpython.com/python-csv/)
