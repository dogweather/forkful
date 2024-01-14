---
title:                "Python: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que
Comparar duas datas pode ser útil em diferentes cenários, como cálculo de diferença entre datas, verificar se uma data está dentro de um determinado período de tempo ou ordenar uma lista de datas.

## Como fazer
Para comparar duas datas em Python, podemos usar o módulo `datetime`. Primeiro, é necessário importá-lo no início do código:
```Python
import datetime
```
Em seguida, podemos criar duas variáveis para armazenar as datas que queremos comparar:
```Python
data1 = datetime.date(2021, 4, 15)
data2 = datetime.date(2021, 4, 20)
```
Agora, podemos usar os operadores de comparação '>' (maior que), '<' (menor que), '==' (igual a), '>=' (maior ou igual a) e '<=' (menor ou igual a) para comparar as datas:
```Python
print(data1 < data2)
# Saída: True
```
Também podemos calcular a diferença entre as duas datas em dias, meses ou anos:
```Python
diferenca = data2 - data1
print(diferenca.days)
# Saída: 5
print(diferenca.months)
# Saída: 0
print(diferenca.years)
# Saída: 0
```

## Aprofundando
O módulo `datetime` também oferece mais funcionalidades para trabalhar com datas, como formatação de datas, manipulação de fuso horário, entre outros. Além disso, é importante lembrar que a comparação de duas datas deve ser feita com cuidado, levando em consideração possíveis diferenças de fuso horário e precisão dos dados.

## Veja também
- [Documentação oficial do módulo datetime](https://docs.python.org/3/library/datetime.html)
- [Guia de referência rápida para o módulo datetime](https://devhints.io/datetime)
- [Tutorial sobre manipulação de datas com Python](https://realpython.com/date-and-time-data/)