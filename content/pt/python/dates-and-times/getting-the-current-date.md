---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.091886-07:00
description: "Buscar a data atual em Python \xE9 uma opera\xE7\xE3o fundamental para\
  \ muitas aplica\xE7\xF5es, como registros (logging), an\xE1lise de dados e tomada\
  \ de decis\xF5es baseadas\u2026"
lastmod: '2024-03-13T22:44:46.164846-06:00'
model: gpt-4-0125-preview
summary: "Buscar a data atual em Python \xE9 uma opera\xE7\xE3o fundamental para muitas\
  \ aplica\xE7\xF5es, como registros (logging), an\xE1lise de dados e tomada de decis\xF5\
  es baseadas no tempo."
title: Obtendo a data atual
weight: 29
---

## Como Fazer:
**Usando a biblioteca padrão `datetime`:**

O módulo `datetime` na biblioteca padrão do Python fornece classes para manipulação de datas e horários. Para obter a data atual, você pode usar o método `date.today()`.

```python
from datetime import date

today = date.today()
print(today)  # Saída: AAAA-MM-DD (ex., 2023-04-05)
```

**Formatando o Tempo:**

Se você precisa da data atual em um formato diferente, o método `strftime` permite especificar uma formatação de data personalizada:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Formato de exemplo: "Abril 05, 2023"
print(formatted_date)
```

**Usando `pendulum` para mais flexibilidade (uma biblioteca de terceiros popular):**

`Pendulum` é uma biblioteca de terceiros que oferece uma abordagem mais intuitiva para lidar com datas e horários em Python. Ela estende as funcionalidades do datetime padrão e simplifica o gerenciamento de fuso horário, entre outras características.

Primeiramente, certifique-se de ter instalado `pendulum` via pip:

```shell
pip install pendulum
```

Então, para obter a data atual:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Saída: AAAA-MM-DD (ex., 2023-04-05)
```

Com `pendulum`, a formatação também é direta e similar à abordagem `strftime`:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Formato padrão: "Abr 5, 2023"
print(formatted_date)
```
