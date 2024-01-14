---
title:                "Python: Convertendo uma data em uma string"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Em programação, muitas vezes encontramos situações em que precisamos lidar com datas e horas. Uma tarefa comum é converter essas datas em uma string, ou seja, em um formato de texto que seja mais fácil de ser lido e manipulado pelo computador. Isso pode ser útil em diversas aplicações, como criar relatórios ou salvar dados em um arquivo de texto. Neste artigo, vamos ver como fazer essa conversão em Python.

## Como fazer

Para converter uma data em uma string em Python, podemos utilizar o pacote `datetime` da biblioteca padrão da linguagem. Primeiro, precisamos importar esse pacote:

```Python
import datetime
```

Em seguida, podemos criar um objeto `datetime` com uma data específica:

```Python
data = datetime.datetime(2021, 8, 17)
```

Agora, podemos utilizar o método `strftime()` para formatar essa data em uma string. Podemos passar como argumento o formato desejado, usando códigos especiais como `%Y` para o ano com quatro dígitos, `%m` para o mês com dois dígitos e `%d` para o dia com dois dígitos:

```Python
data_string = data.strftime("%d/%m/%Y")
print(data_string)
```

O output dessa linha de código será `17/08/2021`, já que definimos a data como 17 de agosto de 2021. Além disso, podemos utilizar outros códigos para incluir informações como o dia da semana, o horário e até mesmo informações em outros idiomas. A documentação completa desses códigos pode ser encontrada na [página da documentação do `strftime`](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior).

## Mergulho profundo

Embora o método `strftime()` seja o mais comumente utilizado para converter uma data em uma string, existem outras opções disponíveis no pacote `datetime`. Algumas delas são:

- `date.strftime()`: semelhante ao método `strftime()` mas aplicado a objetos do tipo `date`, que representam apenas datas sem informações de horário.
- `time.strftime()`: semelhante ao método `strftime()` mas aplicado a objetos do tipo `time`, que representam apenas horários sem informações de data.
- `datetime.strptime()`: faz o processo inverso, ou seja, converte uma string em um objeto `datetime`.
- `datetime.isoformat()`: retorna uma representação em string no formato ISO 8601, que é utilizado como padrão para trocar informações de datas e horários entre diferentes sistemas.

## Veja também

- [Página da documentação do `datetime`](https://docs.python.org/3/library/datetime.html)
- [Página da documentação do `strftime`](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
- [Tutorial da DataCamp sobre como lidar com datas em Python](https://www.datacamp.com/community/tutorials/python-datetime-tutorial)