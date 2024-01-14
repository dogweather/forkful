---
title:                "Python: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-csv.md"
---

{{< edit_this_page >}}

# Por que trabalhar com arquivos CSV em Python?

Trabalhar com arquivos CSV (Comma Separated Values) em Python é uma habilidade importante para qualquer programador, pois esse formato de arquivo é amplamente utilizado em várias áreas, como análise de dados e manipulação de dados em geral. Além disso, o Python possui uma biblioteca padrão robusta para lidar com CSV, tornando-o ainda mais essencial para aqueles que trabalham com dados.

# Como fazer isso:

Para começar a trabalhar com arquivos CSV em Python, você precisará da biblioteca "csv". Para importá-lo, basta escrever o seguinte código no início do seu programa:

```Python 
import csv 
```

Agora, vamos supor que temos um arquivo CSV chamado "dados.csv" com a seguinte estrutura:

| Nome | Sobrenome | Idade |
|------|-----------|-------|
| João | Silva     | 25    |
| Maria| Souza     | 30    |

Para abrir esse arquivo e ler os dados contidos nele, podemos usar a função "reader" da biblioteca csv e um loop for para percorrer cada linha do arquivo. Veja o código abaixo:

```Python 
with open('dados.csv', 'r') as arquivo:
    leitor = csv.reader(arquivo)
    for linha in leitor:
        print(linha)
```

O resultado da execução deste código será a impressão de cada linha do arquivo CSV em uma lista. No nosso exemplo, o output seria o seguinte:

```Python 
['Nome', 'Sobrenome', 'Idade']
['João', 'Silva', '25']
['Maria', 'Souza', '30']
```

Além disso, também é possível escrever em arquivos CSV utilizando a função "writer" da biblioteca csv. Veja o exemplo abaixo:

```Python
with open('dados.csv', 'a') as arquivo:
    escritor = csv.writer(arquivo)
    escritor.writerow(['Pedro', 'Gomes', '40'])
```

Este código irá adicionar a linha "Pedro, Gomes, 40" ao final do nosso arquivo CSV.

# Aprofundando:

A biblioteca csv do Python possui muitas outras funcionalidades, como a capacidade de ler e escrever em arquivos delimitados por outros caracteres além da vírgula, além de oferecer formas de lidar com aspas e espaços nos valores dos arquivos CSV.

Além disso, existem também outras bibliotecas Python populares para trabalhar com CSV, como o "Pandas" e o "NumPy", que oferecem recursos avançados para análise e manipulação de dados em formato CSV.

# Veja também:

- [Documentação oficial da biblioteca csv](https://docs.python.org/3/library/csv.html)
- [Tutorial sobre trabalho com arquivos CSV em Python](https://realpython.com/python-csv/)
- [Pandas, biblioteca de análise de dados em Python](https://pandas.pydata.org/)
- [NumPy, biblioteca científica de computação em Python](https://numpy.org/)