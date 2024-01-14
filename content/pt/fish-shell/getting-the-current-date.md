---
title:                "Fish Shell: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por Que

Ao programar com a Fish Shell, muitas vezes você precisará obter a data atual para realizar diversas tarefas, como nomear arquivos ou criar logs. Felizmente, o Fish Shell tem uma maneira simples e eficiente de obter a data atual sem a necessidade de instalar pacotes adicionais.

# Como Fazer

Para obter a data atual no Fish Shell, você pode utilizar o comando `date`. Veja um exemplo:

```Fish Shell
date +%Y-%m-%d
```

O código acima irá fornecer a data atual no formato "ano-mês-dia" (por exemplo, 2021-08-18). Você também pode personalizar o formato da data adicionando diferentes opções após o comando `date`. Por exemplo:

```Fish Shell
date +%A, %d de %B de %Y
```

Neste caso, o output será algo como "quarta-feira, 18 de agosto de 2021".

# Deep Dive

Por padrão, o comando `date` utiliza a data e hora do sistema. No entanto, você também pode fornecer uma data específica como argumento, utilizando o formato "ano-mês-dia". Por exemplo:

```Fish Shell
date -d 2021-12-25 +%A
```

O output desta linha será "sábado", já que a data fornecida é o Natal de 2021.

Também é possível realizar cálculos simples utilizando o comando `date`. Por exemplo, se você quiser saber qual será a data daqui a 30 dias, pode utilizar o seguinte código:

```Fish Shell
date -d "+30 days" +%A, %d de %B de %Y
```

# Veja Também

- Documentação do comando `date`: https://fishshell.com/docs/current/cmds/date.html
- Tutorial sobre formatação de datas com o Fish Shell: https://arcolinux.com/f-formatting-dates-in-the-terminal-with-the-date-command-in-fish-shell/