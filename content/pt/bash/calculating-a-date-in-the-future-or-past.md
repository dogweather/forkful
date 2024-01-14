---
title:                "Bash: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado é importante?

Muitas vezes, durante o desenvolvimento de um projeto ou na execução de tarefas do dia a dia, pode ser necessário calcular uma data no futuro ou no passado. Isso pode ser útil, por exemplo, para agendar compromissos ou lembretes, ou para manipular dados referentes a um período específico. Neste blog post, vamos explicar como realizar esse cálculo utilizando Bash, uma linguagem de programação bastante popular em sistemas operacionais baseados em Unix.

## Como fazer isso em Bash?

Para calcular uma data no futuro ou passado em Bash, é preciso utilizar a ferramenta `date`. Ela é responsável por manipular e formatar datas e horários. A sintaxe básica para utilizá-la é a seguinte:

```Bash
date "+FORMATO_DE_SAIDA"
```

O `FORMATO_DE_SAIDA` representa a formatação desejada para a data. Por exemplo, se quisermos obter a data atual, podemos utilizar o formato `%d/%m/%Y`, que exibe o dia, mês e ano separados por barras. Assim, o comando completo para esse cálculo ficaria da seguinte forma:

```Bash
date "+%d/%m/%Y"
```

Ao executar esse comando, o output será a data atual no formato especificado. Experimente executar esse comando agora mesmo no seu terminal!

Além disso, para calcular futuras ou passadas, é possível utilizar opções específicas da ferramenta `date`. Por exemplo, para obter a data de hoje mais 1 dia, podemos usar a opção `-d`, seguida do valor `+1 day`. O comando completo ficaria assim:

```Bash
date -d "+1 day" "+%d/%m/%Y"
```

O output será a data de amanhã no formato especificado. Da mesma forma, se quisermos obter a data de hoje menos 1 semana, podemos usar a opção `-d`, seguida do valor `-1 week`. O comando completo ficaria assim:

```Bash
date -d "-1 week" "+%d/%m/%Y"
```

O output será a data de 7 dias atrás no formato especificado.

## Mergulhando mais fundo

Além das opções mencionadas acima, a ferramenta `date` possui outras opções interessantes para realizar cálculos de datas no futuro ou passado. Por exemplo, podemos manipular o valor de anos, meses, semanas e dias no mesmo comando, utilizando as opções `-d`, seguidas dos valores desejados.

Outra opção útil é a possibilidade de utilizar datas específicas como referência. Por exemplo, podemos calcular quantos dias faltam para uma data de aniversário ou para uma data de entrega de um projeto, utilizando a opção `-d`, seguida da data desejada.

Para mais informações e exemplos de utilização da ferramenta `date`, consulte a documentação oficial do Bash.

## Ver também

- Documentação oficial da ferramenta `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Tutorial sobre cálculos de datas com Bash: https://www.baeldung.com/linux/bash-date-commands