---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing de datas em Python

## O Que & Porquê?

Parsing é o processo de decompor uma string data/hora em seus elementos constituintes. Os programadores fazem isso para que possam manipular e analisar datas de maneira mais coerente e apropriada.

## Como Fazer:

Vamos usar o módulo `datetime` do Python. Abaixo, um exemplo de uma string data/hora sendo convertida para um objeto `datetime`.

```Python
from datetime import datetime

minha_data_string = "20/10/2021"
minha_data = datetime.strptime(minha_data_string, "%d/%m/%Y")

print(minha_data)
```

Quando corre essa código, vai receber o seguinte output:

```Python
2021-10-20 00:00:00
```

## Deep Dive

Historicamente, o parsing de datas tem sido um problema notório em muitas linguagens de programação, não apenas em Python. A necessidade de analisar datas de diferentes formatos exigiu o desenvolvimento de módulos robustos e versáteis como o `datetime`.

Python oferece alternativas ao módulo `datetime`, como o módulo `dateutil`, que pode analisar datas de strings em formatos mais complexos.

Em termos de implementação, o método `strptime` do objeto `datetime` usa uma string de formato para determinar a divisão dos elementos em uma string data/hora. Isso permite que ele lide com uma ampla variedade de formatos de data.

## Veja Também

Aqui estão alguns recursos adicionais para ajudá-lo a vice:

1. [Python Datetime](https://docs.python.org/3/library/datetime.html)
2. [Python Dateutil](https://dateutil.readthedocs.io/en/stable/)
3. [Tutorial de datas e horários em Python](https://realpython.com/python-datetime/)
4. [Aprendendo a lidar com datas e horários em Python](https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-dates)