---
title:    "Python: Comparando duas datas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas é útil?

Comparar duas datas é uma tarefa comum em programação. Isso pode ser útil para verificar se uma data é posterior a outra, encontrar o intervalo de tempo entre duas datas ou até mesmo para criar lógicas em sistemas de agendamento. Neste artigo, vamos aprender como comparar duas datas em Python e aprofundar o assunto.

## Como fazer

Para comparar duas datas em Python, primeiro precisamos importar a biblioteca "datetime". Em seguida, podemos criar dois objetos "datetime" com as datas que queremos comparar.

```Python
import datetime

data1 = datetime.datetime(2021, 6, 1)
data2 = datetime.datetime(2021, 7, 1)
```

Agora, podemos usar os operadores de comparação (>, <, ==) para comparar as duas datas. Vamos criar uma estrutura de decisão para verificar se a data1 é anterior, posterior ou igual à data2.

```Python
if data1 > data2:
    print("A data1 é posterior à data2")
elif data1 < data2:
    print("A data1 é anterior à data2")
else:
    print("As datas são iguais")
```

A saída deste código seria "A data1 é anterior à data2". Podemos também usar as funções "date.today()" e "date.fromisoformat()" para criar objetos "datetime" com as datas atuais ou com datas fornecidas pelo usuário.

## Mergulho Profundo

Ao comparar duas datas, é importante ter em mente que o resultado pode variar dependendo da precisão das datas. Por exemplo, se estivermos comparando apenas o dia, mês e ano, datas com horários diferentes podem ser consideradas iguais.

Além disso, quando comparando datas com diferentes formatos, é necessário converter as datas para o mesmo formato antes de compará-las.

Uma forma de fazer isso é usando a função "strftime()" para converter o objeto "datetime" em uma string com o formato desejado. Por exemplo, para comparar duas datas com o formato "dia/mês/ano", podemos fazer o seguinte:

```Python
import datetime

data1 = datetime.datetime(2021, 6, 1)
data2 = datetime.datetime(2021, 7, 1)
data_formatada = data1.strftime("%d/%m/%Y")

if data_formatada == data2.strftime("%d/%m/%Y"):
    print("As datas são iguais")
else:
    print("As datas são diferentes")
```

## Veja também

- [Documentação oficial sobre a biblioteca "datetime" em Python](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre manipulação de datas em Python](https://realpython.com/python-datetime/)
- [Explicação sobre formatação de datas em Python](https://strftime.org/)