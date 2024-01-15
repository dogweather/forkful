---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Python: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em diversas situações, como na programação de eventos ou no planejamento de projetos. Além disso, entender como realizar esses cálculos também pode ajudar a melhorar suas habilidades de programação.

## Como Fazer

Para calcular uma data no futuro ou no passado em Python, é necessário utilizar a biblioteca integrada `datetime`. Aqui está um exemplo de código que mostra como obter a data de 7 dias a frente a partir da data atual:

```Python
from datetime import date, timedelta

today = date.today()
future_date = today + timedelta(days=7)

print(future_date)
```

No código acima, importamos as classes `date` e `timedelta` da biblioteca `datetime`. Em seguida, criamos uma variável `today` que armazena a data atual e adicionamos uma `timedelta` de 7 dias a ela, resultando na variável `future_date`. Por fim, imprimimos o valor de `future_date` que será a data de 7 dias a frente da data atual.

Também é possível calcular uma data no passado, basta utilizar uma `timedelta` negativa. Veja o exemplo abaixo:

```Python
from datetime import date, timedelta

today = date.today()
past_date = today - timedelta(days=7)

print(past_date)
```

Neste caso, adicionamos uma `timedelta` de -7 dias à data atual, resultando na variável `past_date`, que será a data de 7 dias atrás da data atual.

## Deep Dive

Além de dias, a classe `timedelta` também permite calcular datas no futuro ou no passado com base em outras unidades de tempo, como horas, minutos e até mesmo semanas ou meses. Além disso, a biblioteca `datetime` também permite trabalhar com formatos de data diferentes, como ano-mês-dia ou dia/mês/ano.

Para saber mais sobre as possibilidades de cálculos e formatos de data suportados pela biblioteca `datetime`, é recomendado conferir a documentação oficial: https://docs.python.org/3/library/datetime.html

## Veja também

- [Documentação oficial da biblioteca `datetime`](https://docs.python.org/3/library/datetime.html)
- [Tutorial para iniciantes de Python](https://realpython.com/tutorials/getting-started-with-python/)