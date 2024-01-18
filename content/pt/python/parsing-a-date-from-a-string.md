---
title:                "Analisando uma data a partir de uma string."
html_title:           "Python: Analisando uma data a partir de uma string."
simple_title:         "Analisando uma data a partir de uma string."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Parsear uma data a partir de uma string significa extrair informações de uma string que represente uma data, como "Janeiro 1, 2020" ou "20/01/2020". Os programadores geralmente precisam fazer isso para converter uma data em um formato legível para máquinas, como um objeto de data em Python, ou para realizar cálculos e comparações com datas.

## Como fazer:

```Python
# Importar o módulo datetime
import datetime

# Criar uma string com a data
data = "20/01/2020"

# Utilizar o método strptime para parsear a data para um objeto datetime
data_parseada = datetime.datetime.strptime(data, "%d/%m/%Y")

# Imprimir o objeto de data parseada
print(data_parseada)

# Saída: 2020-01-20 00:00:00
```

## Profundando:

Existem várias maneiras de fazer um parsing de data em uma string em Python. Antes do módulo `datetime` ser incluído na biblioteca padrão do Python, os programadores tinham que usar bibliotecas externas, como `dateutil` ou `calendar`, para lidar com datas. Além disso, é importante levar em consideração o formato da string de data ao utilizar o método `strptime` - a documentação do Python fornece uma lista completa de especificadores que podem ser usados para diferentes formatos de data.

## Veja também:

- [Documentação do Python sobre o módulo datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre como trabalhar com datas em Python](https://realpython.com/python-datetime/)
- [Módulo dateutil](https://dateutil.readthedocs.io/en/stable/)