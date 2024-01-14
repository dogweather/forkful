---
title:                "Python: Calculando uma data no futuro ou no passado"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, precisamos saber o dia ou data de algum evento futuro ou passado, seja para planejar ou registrar informações. Com a ajuda da programação em Python, podemos facilmente calcular uma data no futuro ou passado sem precisar contar manualmente nos dedos.

## Como Fazer

Para calcular uma data, primeiro precisamos importar o módulo "datetime" do Python. Em seguida, podemos utilizar a função "timedelta()" para adicionar ou subtrair dias, meses ou anos de uma data específica. Vamos ver um exemplo:

```Python
from datetime import date, timedelta
data_atual = date.today() # data atual
data_futura = data_atual + timedelta(days=30) # adicionar 30 dias
print(data_futura)
```

Neste exemplo, primeiro importamos o módulo "datetime" e definimos a data atual utilizando a função "today()". Em seguida, criamos uma nova variável chamada "data_futura" que armazenará a data atual mais 30 dias, utilizando a função "timedelta()". Por fim, imprimimos a data futura utilizando a função "print()". O output será a data daqui a 30 dias.

Também podemos utilizar valores negativos dentro da função "timedelta()" para subtrair dias, meses ou anos. Por exemplo:

```Python
from datetime import date, timedelta
data_atual = date.today() # data atual
data_passada = data_atual - timedelta(days=15) # subtrair 15 dias
print(data_passada)
```

## Mergulho Profundo

Além de adicionar ou subtrair dias, meses e anos, também podemos utilizar a função "timedelta()" para calcular semanas, horas, minutos e segundos. Basta mudar o parâmetro dentro da função para a unidade desejada, como por exemplo "days=7" para calcular uma semana.

Outra funcionalidade interessante é a possibilidade de utilizar datas diferentes da atual. Por exemplo, podemos criar uma data específica utilizando a função "date()" e utilizar essa data como base para somar ou subtrair dias, meses ou anos utilizando a função "timedelta()".

É importante lembrar que a função "timedelta()" retorna um novo objeto de data, portanto, devemos sempre armazenar o resultado em uma nova variável.

## Veja Também

- Documentação oficial do módulo "datetime": https://docs.python.org/3/library/datetime.html
- Tutorial sobre a utilização do módulo "datetime": https://pt.wikibooks.org/wiki/Python/Conceito_de_datetime
- Exemplos práticos de uso do módulo "datetime": https://www.guru99.com/date-time-and-datetime-classes-in-python.html