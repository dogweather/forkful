---
title:                "Analisando uma data a partir de uma string"
aliases:
- pt/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:00.565448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
Analisar uma data de uma string envolve converter informações textuais de data e hora em um objeto datetime ou formato estruturado equivalente. Isso é comumente realizado para permitir operações de aritmética de datas, comparações e formatações de uma maneira que seja independente de idioma e região. Programadores fazem isso para manipular e tratar eficientemente dados temporais extraídos de logs, entradas de usuários ou fontes externas.

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
