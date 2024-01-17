---
title:                "Comparando duas datas"
html_title:           "Fish Shell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

O que é e por que comparar duas datas?
Comparar duas datas é uma tarefa comum para programadores, pois permite verificar a equivalência ou diferença entre duas datas. Isso é especialmente útil quando se trabalha com dados temporais, como registros de eventos ou vencimento de prazos.

Como fazer:
Existem diferentes maneiras de comparar duas datas no Fish Shell. Uma opção é usar o comando ```date``` para obter a data atual e, em seguida, compará-la com a data desejada. Por exemplo:

```
set data_atual (date +%s) # Obtém a data atual em segundos desde 1970
set data_comparada 1613686800 # Define a data que queremos comparar em segundos desde 1970 
if test $data_atual -gt $data_comparada # Compara as datas utilizando a opção '-gt' (maior que)
    echo "A data atual é posterior à data comparada"
end
```

Outra opção é usar o módulo ```date``` do Fish Shell, que possui funções específicas para comparação de datas. Por exemplo:

```
set data_atual (date +%D) # Obtém a data atual no formato DD/MM/YY 
set data_comparada 18/02/21 # Define a data que queremos comparar 
if date -f %D -d "$data_atual" > date -f %D -d "$data_comparada" # Compara as datas utilizando a função '-f' (formato) e '-d' (data)
    echo "A data atual é posterior à data comparada"
end
```

Mergulho profundo:
Comparar datas pode ser uma tarefa complexa, pois é necessário levar em consideração formatos diferentes, fusos horários, entre outros aspectos. Além das opções mencionadas acima, também é possível utilizar ferramentas externas, como o utilitário ```dateutils```, que permite realizar operações com datas no Fish Shell.

Veja também:
- Documentação oficial do comando ```date```: https://fishshell.com/docs/current/cmds/date.html
- Documentação do módulo ```date```: https://fishshell.com/docs/current/cmds/date.html#date
- Utilitário dateutils: https://github.com/hroptatyr/dateutils