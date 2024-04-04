---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Como fazer: O Python facilita a convers\xE3o de datas em strings. Use\
  \ o m\xE9todo [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-\u2026"
lastmod: '2024-04-04T02:02:34.895467-06:00'
model: gpt-4-0125-preview
summary: "O Python facilita a convers\xE3o de datas em strings."
title: Convertendo uma data em uma string
weight: 28
---

## Como fazer:
O Python facilita a conversão de datas em strings. Use o método [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) disponível em objetos de [data](https://docs.python.org/3/library/datetime.html#date-objects). Veja como:

```Python
from datetime import datetime

# Obter a data e a hora atuais
now = datetime.now()

# Converta para uma string no formato: Mês dia, Ano
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Saída: 29 de março de 2023 (ou data atual)

# Formato: AAAA-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Saída: 2023-03-29 (ou data atual)
```

### Como eu faço

É assim que obtenho uma data em formato [ISO 8601](https://www.w3.org/QA/Tips/iso-date) com informações de fuso horário:

```python
def datestamp() -> str:
    """ 
    A data e a hora atual com fuso horário em formato ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Exemplo de saída:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```

## Aprofundando
Historicamente, a conversão de datas em strings tem sido fundamental na programação devido à necessidade de representar datas em um formato legível por humanos.

Alternativas ao `strftime` incluem o uso do método `isoformat` para o formato ISO 8601, ou bibliotecas de terceiros como `arrow` e `dateutil` que oferecem opções de formatação e análise mais flexíveis.

Em termos de implementação, `strftime` significa "string format time" (formato de hora em string) e tem suas raízes na programação C. O `strftime` do Python interpreta códigos de formatação como `%Y` para o ano e `%m` para o mês, permitindo uma personalização quase infinita.

## Veja também
Para se aprofundar nas funções de data e hora do Python:
- Documentação oficial do `datetime` do Python: https://docs.python.org/3/library/datetime.html
- Para aqueles interessados em uma lista completa de diretivas `strftime`: https://strftime.org/
- Para explorar bibliotecas de terceiros sobre data/hora:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
