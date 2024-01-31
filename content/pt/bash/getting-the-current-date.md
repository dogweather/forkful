---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:12:50.280970-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Todo mundo sabe o que é a data atual: é a informação do dia, mês e ano em que estamos. Programadores frequentemente a usam para registrar eventos, gerar relatórios ou controlar prazos.

## Como Fazer:
```Bash
# Comando simples para pegar a data e hora atual:
date

# Exemplo de saída:
Thu Mar 23 10:30:42 AM PDT 2023

# Formatar a data para o padrão brasileiro (dia/mês/ano):
date +%d/%m/%Y

# Exemplo de saída:
23/03/2023

# Pegar apenas a hora:
date +%H:%M

# Exemplo de saída:
10:30
```
## Mergulho Profundo
Lidar com datas é uma necessidade antiga nos sistemas *nix. O comando `date` do Unix existe desde os primeiros dias do sistema. Alternativas incluem o uso de programas como `awk` ou `perl` para manipulação de datas, mas o `date` mantém sua posição pela simplicidade. A especificação de formato `+%Y-%m-%d`, por exemplo, é influenciada pela norma ISO 8601 para representação de datas e horas.

Diferentes sistemas podem ter pequenas variações em seus comandos `date`, então é importante consultar a página do manual (`man date`) para detalhes específicos da implementação. No Bash, data e hora são importantes em scripts para marcações de tempo (`timestamps`), operações de cron (agendamento de tarefas), e em muitos outros casos onde a temporalidade é relevante.

## Veja Também
- Manual do Bash (`man bash` no terminal)
- Guia de utilização do `date`: https://ss64.com/bash/date.html
- Formatação do comando `date`: http://man7.org/linux/man-pages/man1/date.1.html
- História dos sistemas Unix: https://en.wikipedia.org/wiki/History_of_Unix
