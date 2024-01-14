---
title:                "Python: Comparando duas datas"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar datas é uma tarefa comum em programação, especialmente quando se trabalha com dados de séries temporais ou eventos. Ao comparar duas datas, podemos determinar a diferença entre elas, verificar se estão em uma ordem específica ou até mesmo calcular o intervalo entre elas. É uma habilidade importante a ser dominada por programadores de Python.

## Como fazer a comparação

Para comparar duas datas em Python, podemos utilizar o módulo "datetime". Primeiro, precisamos importar o módulo:
```Python
import datetime
```

Em seguida, podemos definir as datas que desejamos comparar, utilizando a classe "datetime":
```Python
data_1 = datetime.datetime(2021, 5, 15)
data_2 = datetime.datetime(2021, 5, 20)
```

Agora, podemos utilizar os operadores de comparação para verificar a relação entre as datas. Por exemplo, para verificar se a data_1 é anterior à data_2, podemos utilizar o operador "less than" (<):
```Python
if data_1 < data_2:
    print("A data_1 é anterior à data_2")
```

Também podemos utilizar o operador "greater than" (>) para verificar se uma data é posterior a outra. E se quisermos verificar se as datas são iguais, podemos utilizar o operador de igualdade (==).

## Aprofundando na comparação de datas

Ao comparar datas em Python, é importante ter em mente que existem alguns detalhes a serem considerados. Por exemplo, se estivermos comparando datas com precisão de horas, minutos ou segundos, devemos utilizar a classe "datetime" e especificar esses valores ao definir as datas. Além disso, devemos levar em conta que datas são objetos imutáveis, ou seja, não podemos alterá-las diretamente.

Também é importante notar que o módulo "datetime" possui métodos úteis para manipular datas, como o método "strftime" que permite formatar uma data de acordo com um padrão específico, e o método "timedelta" que permite adicionar ou subtrair um intervalo de tempo de uma data.

Ao aprofundar na comparação de datas, é essencial ler a documentação oficial do módulo"datetime" e praticar com diferentes exemplos para melhorar a compreensão dessa habilidade.

## Veja também

- [Documentação oficial do módulo datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre o módulo datetime](https://www.w3schools.com/python/python_datetime.asp)
- [Perguntas frequentes sobre datas em Python](https://medium.com/swlh/python-datetime-module-a-foray-into-comparing-dates-with-timezones-df622fb024f6)