---
title:                "Calculando uma data no futuro ou passado"
aliases:
- /pt/python/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:42.520475-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Calcular uma data futura ou passada significa simplesmente adicionar ou subtrair dias, meses ou anos de uma data específica. Programadores fazem isso para funções como agendar eventos, gerar lembretes, ou calcular prazos.

## Como Fazer:
```Python
from datetime import datetime, timedelta

# Data atual
hoje = datetime.now()

# Calculando uma data no futuro (+10 dias)
futuro = hoje + timedelta(days=10)
print(f"Futuro: {futuro.strftime('%d/%m/%Y')}")

# Calculando uma data no passado (-30 dias)
passado = hoje - timedelta(days=30)
print(f"Passado: {passado.strftime('%d/%m/%Y')}")
```
Saída:
```
Futuro: DD/MM/AAAA
Passado: DD/MM/AAAA
```
Troque `DD/MM/AAAA` pelas datas correspondentes.

## Mergulho Profundo
Historicamente, a manipulação de datas tem sido um desafio em programação devido a variações de calendário, fusos horários e regras de horário de verão. No Python, o módulo `datetime` é uma abstração que simplifica essas operações.

Alternativas incluem usar o `dateutil.relativedelta` para necessidades mais complexas, como acrescentar meses ou anos, considerando as diferenças em número de dias por mês:

```Python
from datetime import datetime
from dateutil.relativedelta import relativedelta

# Adicionando 1 mês à data atual
um_mes_a_frente = datetime.now() + relativedelta(months=1)
```

As implementações para cálculo de datas no futuro ou passado devem sempre levar em conta exceções, como anos bissextos ou diferenças culturais em relação à definição da primeira semana do ano e o primeiro dia da semana.

## Veja Também
- Documentação oficial do módulo `datetime`: https://docs.python.org/3/library/datetime.html
- Pacote `dateutil`: https://dateutil.readthedocs.io/en/stable/
- Questões de tempo e data no Stack Overflow: https://stackoverflow.com/questions/tagged/datetime
