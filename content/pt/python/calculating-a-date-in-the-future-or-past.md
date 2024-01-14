---
title:                "Python: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como seria saber qual dia da semana será daqui a 5 anos? Ou talvez quantos dias faltam para o seu aniversário no próximo ano? Aprender a programar em Python pode te ajudar a responder essas perguntas e muito mais! Neste artigo, vamos te ensinar como calcular datas no futuro ou no passado utilizando Python.

## Como Fazer

Para realizar cálculos de datas em Python, você irá precisar utilizar o módulo `datetime`. Primeiro, vamos importá-lo em nosso código:

```Python
import datetime
```

Para calcular uma data no futuro, podemos utilizar o método `today()` do módulo `date` para obter a data atual. Em seguida, utilizamos o método `timedelta()` para adicionar um determinado número de dias à data atual. Por exemplo, para calcular a data daqui a 30 dias:

```Python
data_atual = datetime.date.today()
data_futura = data_atual + datetime.timedelta(days=30)
```

Podemos também utilizar o método `strftime()` para formatar a data em uma string legível. No exemplo acima, a data seria formatada como "ano/mês/dia". Confira um exemplo completo para calcular a data daqui a 30 dias:

```Python
import datetime

data_atual = datetime.date.today()
data_futura = data_atual + datetime.timedelta(days=30)
data_formatada = data_futura.strftime('%d/%m/%Y')

print('Data atual:', data_atual)
print('Data futura:', data_futura)
print('Data formatada:', data_formatada)
```

A saída seria:

```Output
Data atual: 2020-09-22
Data futura: 2020-10-22
Data formatada: 22/10/2020
```

Para calcular uma data no passado, apenas mudamos o sinal do número de dias para negativo. Por exemplo, para obter a data de 30 dias atrás:

```Python
data_atual = datetime.date.today()
data_passada = data_atual + datetime.timedelta(days=-30)
```

E para formatar a data:

```Python
data_formatada = data_passada.strftime('%d/%m/%Y')
```

## Mergulho Mais Profundo

Além de adicionar ou subtrair um número de dias, podemos utilizar o método `replace()` para alterar outros componentes da data, como o ano, mês e dia. Por exemplo, para calcular a data de 1 ano e meio atrás:

```Python
data_atual = datetime.date.today()
data_passada = data_atual.replace(year=data_atual.year-1, months=data_atual.month-6)
```

Também podemos fazer cálculos com relação a horários utilizando o módulo `timedelta`. Por exemplo, para obter a data e hora atuais e adicionar 2 horas e 30 minutos, podemos realizar o seguinte:

```Python
import datetime

data_hora_atual = datetime.datetime.now()
data_hora_futura = data_hora_atual + datetime.timedelta(hours=2, minutes=30)

print('Data e hora atual:', data_hora_atual)
print('Data e hora futura:', data_hora_futura)
```

E a saída seria:

```Output
Data e hora atual: 2020-09-22 15:40:00.654182
Data e hora futura: 2020-09-22 18:10:00.654182
```

## Veja Também

- Documentação do módulo `datetime`: https://docs.python.org/3/library/datetime.html
- Tutorial sobre manipulação de datas em Python: https://strftime.org/