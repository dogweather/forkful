---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Converter uma data em uma string, em Python, significa transformar um objeto de data em um formato de texto. Programadores fazem isso para facilitar o manuseio e apresentação de datas, principalmente quando precisam ser exibidas para o usuário.

## Como Fazer:
Para converter uma data em uma string, usamos a função `strftime()`, que faz parte do módulo `datetime` em Python. Veja a seguir um exemplo:

```Python
from datetime import datetime

# Criar uma data
data = datetime(2022, 3, 26, 9, 30)

# Converter a data em string
data_em_str = data.strftime('%d/%m/%Y %H:%M')

print(data_em_str)
```

Quando executamos este código, obtemos a seguinte saída:

```
26/03/2022 09:30
```

## Mergulho Profundo 

Originalmente, a função `strftime()` foi introduzida no módulo `time` do Python 1.5.2 em 1999 e, posteriormente, adicionada ao módulo `datetime` no Python 2.3, pois fornece uma maneira mais orientada a objetos de manipular datas e horários.

Como alternativa à função `strftime()`, também podemos usar a função `isoformat()` para converter um objeto de data em uma string no formato ISO 8601:

```Python
data_em_str = data.isoformat()
print(data_em_str)
```

A implementação detalhada do `strftime()` é bastante complexa, pois precisa lidar com uma variedade de formatos e zonas horárias. Felizmente, o Python simplifica este processo para nós, fornecendo um método fácil de usar para converter datas em strings.

## Veja Também

Para mais informações, visite os recursos a seguir:

1. Documentação oficial do Python sobre o módulo datetime: https://docs.python.org/3/library/datetime.html
2. Stack Overflow: Como formatar uma data em Python: https://stackoverflow.com/questions/10624937/convert-datetime-object-to-a-string-of-date-only-in-python
3. W3Schools: Python DateTime strftime() Method: https://www.w3schools.com/python/ref_datetime_date_strftime.asp