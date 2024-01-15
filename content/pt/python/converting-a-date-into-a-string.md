---
title:                "Convertendo uma data em uma cadeia de caracteres"
html_title:           "Python: Convertendo uma data em uma cadeia de caracteres"
simple_title:         "Convertendo uma data em uma cadeia de caracteres"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

 Converter uma data em uma string é uma tarefa comum na programação, pois permite que programadores apresentem informações de datas de forma mais legível para os usuários finais ou para armazenamento em bancos de dados.

## Como fazer

O método mais comum para converter uma data em uma string é usando o módulo "datetime" do Python. Primeiro, precisamos importar o módulo e criar uma variável com a data desejada. Por exemplo:

```Python
import datetime
data = datetime.date(2021, 11, 15)
```

Agora, podemos usar o método "strftime" para formatar a data em uma string específica. Por exemplo, se quisermos apresentar a data no formato "dia/mês/ano", usamos o código:

```Python
print(data.strftime("%d/%m/%Y"))
```
E a saída será "15/11/2021". Existem diversas opções de formatação, onde cada letra representa uma informação da data (dia, mês, ano, hora, etc). Você pode encontrar mais opções [aqui](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes).

## Mergulho Profundo

Além do método "strftime", existem outras maneiras de converter datas em strings no Python. Por exemplo, podemos usar a função "format" para formatar a data de acordo com uma máscara específica. Por exemplo:

```Python
print("{:%d/%m/%y}".format(data))
```

A saída será "15/11/21". Também podemos converter diretamente uma string em uma data usando o método "strptime". Por exemplo:

```Python
data_string = "15/11/2021"
nova_data = datetime.datetime.strptime(data_string, "%d/%m/%Y")
print(nova_data)
```

A saída será "2021-11-15 00:00:00". É importante lembrar de usar a mesma máscara de formatação tanto para o método "strftime" quanto para o método "strptime".

## Veja também
- [Documentação oficial sobre a biblioteca datetime do Python](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre formatação de datas em Python](https://realpython.com/python-datetime/)
- [Diferenças entre datetime, date, time e timedelta em Python](https://stackoverflow.com/questions/58824092/differences-between-datetime-date-time-and-timedelta)