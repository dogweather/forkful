---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Comparar duas datas em programação é verificar qual é anterior, posterior ou se são iguais. Programadores fazem isso para controlar eventos dependentes do tempo, como expirações de assinatura, agendamentos ou marcos de projeto.

## Como Fazer:
Para comparar duas datas em Python, você precisa usar o módulo `datetime`. Veja o exemplo a seguir:
```Python
from datetime import datetime

# Datetime no formato AAAA-MM-DD
data1 = datetime.strptime('2021-10-01', '%Y-%m-%d')
data2 = datetime.strptime('2021-12-01', '%Y-%m-%d')

# Comparação
if data1 < data2:
    print("data1 é antes de data2")
elif data1 > data2:
    print("data1 é depois de data2")
else:
    print("data1 é igual a data2")
```
A execução deste código imprimirá "data1 é antes de data2", pois 1 de outubro de 2021 é anterior a 1 de dezembro de 2021.

## Mergulhando Fundo
No contexto histórico, as datas sempre foram uma parte crucial dos sistemas de software. No Python, a biblioteca `datetime` nos ajudou a resolver muitos problemas relacionados a datas desde seu lançamento com Python 2.3 em 2003.

Como alternativa à biblioteca `datetime`, você pode usar a biblioteca `arrow` ou `pendulum`, que oferecem uma API mais amigável e muitas outras funcionalidades.

No que diz respeito à implementação, `datetime.strptime` converte uma string para um objeto datetime. Os operadores de comparação (>, <, ==) são sobrecarregados para funcionar com objetos datetime, o que facilita as comparações de datas.

## Veja Também
Documentação oficial do Python sobre o `datetime`: 
https://docs.python.org/3/library/datetime.html

Documentação da biblioteca `arrow` para manipulação de datas:
https://arrow.readthedocs.io/en/latest/

Documentação da biblioteca `pendulum` para manipulação de datas:
https://pendulum.eustace.io/docs/