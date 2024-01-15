---
title:                "Comparando duas datas"
html_title:           "Python: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como comparar uma data com outra em Python? Bem, há diversas razões pelas quais isso pode ser útil, como comparar a data atual com uma data de entrada do usuário ou verificar se uma data específica ocorreu antes ou depois de outra.

## Como fazer

Para comparar datas em Python, usamos o módulo `datetime`, que nos permite trabalhar com objetos de data e hora. Primeiro, vamos importá-lo em nosso código:

```Python
import datetime
```

Agora, vamos criar duas variáveis que representam as datas que queremos comparar:

```Python
data1 = datetime.date(2020, 6, 15)
data2 = datetime.date(2021, 1, 1)
```

Em seguida, podemos usar os operadores de comparação para verificar a relação entre as duas datas. Por exemplo, se quisermos verificar se `data1` ocorreu antes de `data2`, podemos usar o operador `<` da seguinte forma:

```Python
if data1 < data2:
    print("data1 ocorreu antes de data2")
```

Da mesma forma, podemos usar o operador `>` para verificar se uma data ocorreu depois de outra. Se quisermos verificar se as datas são iguais, usamos o operador `==`:

```Python
if data1 == data2:
    print("As datas são iguais")
```

Podemos até mesmo usar os operadores de comparação em conjunto com outras operações para fazer comparações mais complexas. Por exemplo, podemos verificar se uma data está dentro de um determinado intervalo:

```Python
data3 = datetime.date(2020, 1, 1)
if data1 > data3 and data1 < data2:
    print("data1 está entre as datas de data3 e data2")
```

## Mergulho profundo

O módulo `datetime` oferece uma variedade de funções e métodos que nos permitem trabalhar com datas de forma eficaz. Por exemplo, podemos usar o método `weekday()` para obter o dia da semana em que uma determinada data cai. O dia da semana é representado como um número inteiro, sendo 0 para segunda-feira e 6 para domingo.

```Python
data4 = datetime.date(2020, 6, 18)
dia_da_semana = data4.weekday()
print(dia_da_semana) # output: 3 (quinta-feira)
```

Além disso, o módulo `datetime` também possui uma classe `timedelta` que nos permite realizar operações com intervalos de tempo. Por exemplo, se quisermos calcular a diferença entre duas datas, podemos criar um objeto `timedelta` a partir delas e imprimir o resultado:

```Python
intervalo = data2 - data1
print(intervalo) # output: 200 dias, 0:00:00
```

Essas são apenas algumas das muitas funcionalidades que o módulo `datetime` nos oferece. Explore a documentação oficial para descobrir mais maneiras de trabalhar com datas em Python.

## Veja também

- Documentação oficial do módulo `datetime`: https://docs.python.org/3/library/datetime.html
- Guia de referência para o módulo `datetime`: https://www.programiz.com/python-programming/datetime
- Vídeo tutorial sobre como trabalhar com datas em Python: https://www.youtube.com/watch?v=zB28makjTso