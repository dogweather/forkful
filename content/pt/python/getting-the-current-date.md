---
title:                "Python: Obtendo a data atual"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que?

Se você é iniciante em programação ou até mesmo um profissional experiente, provavelmente já se deparou com a necessidade de obter a data atual em seu código. Afinal, a data é uma informação crucial para muitas aplicações, desde controlar o tempo de criação de um arquivo até mostrar a data de postagem em um blog. Sabendo disso, neste artigo explicaremos como obter a data atual em Python e por que isso é tão importante.

## Como Fazer
Para obter a data atual em Python, podemos utilizar o módulo built-in `datetime` e a função `date.today()`. Veja o exemplo abaixo:

```Python
from datetime import date

data_atual = date.today()
print(data_atual)
```

Output:
```
2021-03-14
```

Neste código, importamos o módulo `datetime` que contém funções específicas para lidar com datas e horários. Em seguida, utilizamos a função `date.today()` para obter a data atual e atribuímos o resultado à variável `data_atual`. Por fim, imprimimos a variável para exibir a data atual no formato `ano-mês-dia`.

Além disso, também é possível formatar a saída da data utilizando o método `strftime()` do módulo `datetime`. Veja o exemplo abaixo:

```Python
from datetime import date

data_atual = date.today()
data_formatada = data_atual.strftime('%d/%m/%Y')
print(data_formatada)
```

Output:
```
14/03/2021
```

Neste caso, utilizamos `%d` para representar o dia, `%m` para o mês e `%Y` para o ano. Dessa forma, a saída será a data atual no formato `dia/mês/ano`.

## Deep Dive

A função `date.today()` retorna a data atual com base no fuso horário do sistema em que o código está sendo executado. No entanto, se precisarmos obter a data e hora em um fuso horário específico, podemos utilizar o objeto `datetime` e a função `now()` com o parâmetro `tz` (timezone). Veja o exemplo abaixo:

```Python
from datetime import datetime
from pytz import timezone

fuso_horario = timezone('America/Sao_Paulo')
data_atual = datetime.now(tz=fuso_horario)
print(data_atual)
```

Output:
```
2021-03-14 11:30:00.921820-03:00
```

Neste exemplo, importamos o módulo `datetime` e também o `pytz`, que permite trabalhar com fusos horários específicos. Em seguida, definimos o fuso horário desejado (`America/Sao_Paulo`) e utilizamos a função `now()` para obter a data e hora atual com base nesse fuso horário. O resultado é um objeto `datetime` com a data e hora juntamente com o fuso horário.

## Veja também
- [Documentação do módulo datetime do Python](https://docs.python.org/3/library/datetime.html)
- [Lista de fusos horários suportados pelo pytz](https://gist.github.com/heyalexej/8bf688fd67d7199be4a1682b3eec7568)
- [Tutorial sobre datetime em Python](https://realpython.com/python-datetime/)

Esperamos que este artigo tenha sido útil e que você possa facilmente obter a data atual em seus projetos em Python. Não deixe de conferir os links adicionais para aprofundar seus conhecimentos. Até a próxima!