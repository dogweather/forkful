---
title:    "Bash: Calculando uma data no futuro ou passado"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

O cálculo de datas no Bash pode ser útil para tarefas como agendar tarefas futuras ou lidar com arquivos que foram modificados em uma data específica no passado. Ao entender como isso funciona, você pode tornar seus scripts ainda mais robustos e automatizados.

## Como fazer

Para calcular uma data no futuro ou no passado no Bash, você precisa entender os formatos de data e hora utilizados pelo sistema operacional. Geralmente, o Bash usa a variável $EPOCHSECONDS, que representa o número total de segundos desde 1º de janeiro de 1970. A partir disso, podemos adicionar ou subtrair a quantidade de segundos necessária para chegar à data desejada.

Por exemplo, para calcular uma data daqui a 5 dias:

```Bash
date -d "$(date -d "+5 days")" +%m/%d/%Y
```
Isso retornará a data exata de 5 dias a partir da data atual, em um formato de mês/dia/ano.

Para calcular uma data no passado, basta substituir o "+5 days" por "-5 days". Você também pode especificar um horário específico usando o formato de 24 horas:

```Bash
date -d "$(date -d "10:00 yesterday")" +%m/%d/%Y
```
Isso retornará a data de ontem às 10h da manhã, no mesmo formato.

## Investigação mais profunda

Além dos formatos de data e hora, também é possível utilizar a ferramenta "cal" do Bash para calcular datas. Esta ferramenta mostra o calendário em um determinado mês e ano e também pode ser usada para encontrar datas específicas:

```Bash
cal 10 2021
cal 3 2023
```

Esses comandos retornarão o calendário para outubro de 2021 e março de 2023, respectivamente. Além disso, você pode especificar um dia específico para ver se ele cai em um determinado dia da semana:

```Bash
cal 10 2021 | grep "Fri"
```

Isso mostrará todas as sextas-feiras do mês de outubro de 2021.

## Veja também

- [Documentação Bash sobre a variável $EPOCHSECONDS](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables)
- [Mais sobre o comando "date"](https://www.cyberciti.biz/tips/linux-unix-get-yesterdays-tomorrows-date.html)
- [Explicação detalhada sobre a ferramenta "cal"](https://www.linux.com/training-tutorials/accessing-calendars-bash/)