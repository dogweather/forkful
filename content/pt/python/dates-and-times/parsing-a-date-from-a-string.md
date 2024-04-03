---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:00.565448-07:00
description: "Como fazer: A biblioteca padr\xE3o do Python fornece o m\xF3dulo `datetime`,\
  \ que inclui o m\xE9todo `strptime` para essa finalidade. O m\xE9todo requer dois\u2026"
lastmod: '2024-03-13T22:44:46.163726-06:00'
model: gpt-4-0125-preview
summary: "A biblioteca padr\xE3o do Python fornece o m\xF3dulo `datetime`, que inclui\
  \ o m\xE9todo `strptime` para essa finalidade."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como fazer:
A biblioteca padrão do Python fornece o módulo `datetime`, que inclui o método `strptime` para essa finalidade. O método requer dois argumentos: a string da data e uma diretiva de formato que especifica o padrão da string de entrada.

```python
from datetime import datetime

# String de exemplo
date_string = "2023-04-01 14:30:00"
# Analisando string para o objeto datetime
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Saída: 2023-04-01 14:30:00
```

Para uma análise de datas mais matizada, especialmente ao lidar com múltiplos formatos ou localidades, a biblioteca de terceiros `dateutil` pode ser extremamente útil. Ela fornece um módulo parser que pode analisar datas em quase qualquer formato de string.

```python
from dateutil import parser

# Strings de exemplo
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Usando o parser do dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Saída: 2023-04-01 14:30:00
print(parsed_date2)
# Saída: 2023-04-01 14:30:00
```

O `dateutil` é habilidoso em lidar com a maioria dos formatos de data sem strings de formato explícitas, tornando-o uma escolha versátil para aplicações que lidam com representações diversificadas de datas.
