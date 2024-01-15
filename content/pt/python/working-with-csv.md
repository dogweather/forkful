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

## Por que
Antes de mergulharmos na linguagem de programação Python e como ela pode ser usada para trabalhar com arquivos CSV, é importante entendermos o motivo pelo qual isso pode ser benéfico para você. CSV (Comma Separated Values) é um formato de arquivo amplamente utilizado para armazenar e manipular dados tabulares, tornando-o um padrão de fato para muitas tarefas de análise de dados. Trabalhar com CSVs pode facilitar muito a organização e manipulação de dados em diferentes projetos, além de permitir que você compartilhe facilmente esses dados com outras pessoas ou sistemas.

## Como Fazer
Para começar a trabalhar com CSVs em Python, é necessário primeiro importar o módulo `csv` para poder acessar suas funções úteis. Em seguida, você pode abrir o arquivo CSV desejado usando a função `open()` e especificando o modo de leitura. Por exemplo, se quisermos ler o arquivo "dados.csv", podemos usar o seguinte código:

```Python
import csv

with open("dados.csv", "r") as arquivo:
    leitor = csv.reader(arquivo)
    for linha in leitor:
        print(linha)
```

Este código criará um objeto de leitor que podemos iterar para obter cada linha do arquivo CSV. Podemos manipular e usar esses dados de acordo com nossas necessidades.

## Profundando
Embora o exemplo acima seja um bom ponto de partida para trabalhar com arquivos CSV em Python, existem muitas outras funções e opções disponíveis no módulo `csv`. Podemos usar diferentes delimitadores, como tabulações ou vírgulas, através do parâmetro `delimiter` e também especificar o caractere de escapamento através do parâmetro `escapechar`. Além disso, podemos até mesmo escrever em arquivos CSV usando a função `writer()`. Ter um bom entendimento de como essas funções funcionam e como manipular os dados dentro de um arquivo CSV pode ser muito útil ao trabalhar com conjuntos de dados grandes e complexos.

## Veja Também
- [Documentação oficial do módulo csv do Python](https://docs.python.org/3/library/csv.html)
- [Tutorial da W3Schools sobre como trabalhar com arquivos CSV em Python](https://www.w3schools.com/python/python_csv.asp)
- [Tutorial do Real Python sobre como ler e escrever em arquivos CSV em Python](https://realpython.com/python-csv/)