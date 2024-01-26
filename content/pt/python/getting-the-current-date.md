---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:14.450368-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Obter a data atual no Python é pegar a informação exata do dia em que estamos, incluindo ano, mês e dia. Programadores fazem isso para marcar eventos, medir durações ou controlar quando algo deve acontecer.

## Como Fazer:

```Python
from datetime import datetime

# Obtendo a data e hora atuais
agora = datetime.now()

# Imprimindo a data e hora
print("Data e hora atual:", agora)

# Formatação para obter apenas a data
data_atual = agora.date()
print("Apenas a data atual:", data_atual)
```

Output esperado:

```
Data e hora atual: 2023-04-05 14:23:36.123456
Apenas a data atual: 2023-04-05
```

## Mergulho Profundo:

Historicamente, manejar datas e horários em programação sempre foi um desafio, com questões como fuso horário e anos bissextos complicando o cenário. No Python, a biblioteca `datetime` existe desde as versões iniciais e simplifica muito esse processo.

Alternativas à `datetime` incluem o módulo `time`, que também fornece funções para trabalhar com tempo, mas com uma interface mais voltada ao estilo C. Há ainda o pacote `dateutil`, que estende a `datetime` com capacidades adicionais, como parsing de datas de maneira mais robusta e manipulação de fusos horários.

Quanto à implementação, a `datetime.now()` pega o tempo do sistema em que o código está rodando. A precisão e exatidão dependem do sistema operacional e do hardware.

## Veja Também:

- Documentação oficial do módulo datetime: https://docs.python.org/3/library/datetime.html
- PyPI page do `python-dateutil`: https://pypi.org/project/python-dateutil/
- W3Schools Python DateTime Tutorial: https://www.w3schools.com/python/python_datetime.asp
