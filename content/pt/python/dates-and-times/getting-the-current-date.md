---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.091886-07:00
description: "Como Fazer: **Usando a biblioteca padr\xE3o `datetime`:** O m\xF3dulo\
  \ `datetime` na biblioteca padr\xE3o do Python fornece classes para manipula\xE7\
  \xE3o de datas e\u2026"
lastmod: '2024-03-13T22:44:46.164846-06:00'
model: gpt-4-0125-preview
summary: "**Usando a biblioteca padr\xE3o `datetime`:**\n\nO m\xF3dulo `datetime`\
  \ na biblioteca padr\xE3o do Python fornece classes para manipula\xE7\xE3o de datas\
  \ e hor\xE1rios."
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
