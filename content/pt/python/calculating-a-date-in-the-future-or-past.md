---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Python: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Calcular uma data no futuro ou no passado é determinar exatamente que dia será (ou foi) depois (ou antes) de um determinado período de tempo. Os programadores fazem isso frequentemente em agendamentos, lembretes ou funções de tempo real.

## Como Fazer:
Python fornece uma biblioteca `datetime` para manipular datas. Uma maneira simples de calcular uma data futura é usar o método `timedelta`:

```Python
from datetime import datetime, timedelta

hoje = datetime.now()
futuro = hoje + timedelta(days=10)

print("Hoje é: ", hoje)
print("Daqui a 10 dias será: ", futuro)
```
Quando você executar este código, verá a data atual e a data após 10 dias.

## A Profundidade
Históricamente, a manipulação de datas e horas tem sido uma tarefa complexa devido à definição irregular e política do tempo. Houve necessidade de um modelo uniforme, resultando no modelo Gregoriano que Python usa.

Existem alternatives para a biblioteca `datetime`, como a biblioteca `arrow`, que oferece uma abordagem mais amigável e 'Pythonic'. No entanto, `datetime` permanece popular devido à sua inclusão na biblioteca padrão de Python.

Para entender completamente o cálculo de datas, é importante conhecer a implementação dos objetos `datetime` e `timedelta`. `datetime` representa um único ponto no tempo, enquanto `timedelta` representa uma diferença entre duas datas ou horas.

## Veja Também
- Documentação oficial do Python sobre a biblioteca `datetime`: https://docs.python.org/pt-br/3/library/datetime.html
- Tutorial sobre como trabalhar com datas e horas em Python: https://realpython.com/python-datetime/
- Guia de usuário da biblioteca `arrow`: https://arrow.readthedocs.io/en/latest/