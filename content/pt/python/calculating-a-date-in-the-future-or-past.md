---
title:    "Python: Calculando uma data no futuro ou passado"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas futuras ou passadas é uma habilidade importante para programadores em qualquer linguagem de programação. Isso permite que você automatize tarefas baseadas em datas e melhore a eficiência do seu código.

## Como fazer

Para calcular uma data no futuro ou passado, usamos a biblioteca `datetime` do Python. Primeiro, precisamos importar essa biblioteca no início do nosso código:

```Python
import datetime
```

Em seguida, podemos criar um objeto `datetime` com uma data específica usando a função `datetime()` e passando os argumentos `ano`, `mês` e `dia`:

```Python
data = datetime.datetime(2021, 9, 1)
```

Agora, podemos realizar cálculos de datas utilizando o objeto `data`. Por exemplo, se quisermos calcular a data de 30 dias após a data atual, podemos usar a função `timedelta()` da seguinte forma:

```Python
data_futura = data + datetime.timedelta(days=30)
```

Podemos então imprimir a data futura formatada para exibição ao usuário:

```Python
print(f"Daqui a 30 dias será: {data_futura.strftime('%d/%m/%Y')}")
```

A saída será:

```
Daqui a 30 dias será: 01/10/2021
```

## Deep Dive

Além de adicionar dias, podemos realizar diversos outros cálculos de datas, como subtrair dias, adicionar ou subtrair semanas, meses e anos, e até mesmo encontrar a diferença de dias entre duas datas. A biblioteca `datetime` possui diversas funções e métodos que permitem manipular e formatar datas de maneiras diferentes, tornando-a uma ferramenta poderosa para trabalhar com datas em programas Python.

## Veja também

- [Documentação oficial do módulo `datetime` do Python](https://docs.python.org/pt-br/3/library/datetime.html)
- [Tutorial sobre manipulação de datas em Python](https://realpython.com/python-datetime/)
- [Exemplos práticos de cálculo de datas em Python](https://www.w3schools.com/python/python_datetime.asp)