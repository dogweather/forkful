---
title:                "Calculando uma data no futuro ou no passado."
html_title:           "Fish Shell: Calculando uma data no futuro ou no passado."
simple_title:         "Calculando uma data no futuro ou no passado."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

O que & Porquê?

Calcular uma data no futuro ou passado é uma tarefa comum na programação. Isso envolve usar uma linguagem de programação para determinar ou prever uma data específica, com base em informações fornecidas pelo usuário. Os programadores muitas vezes precisam calcular datas para agendar tarefas, definir prazos ou realizar tarefas de datação em aplicativos.

Como fazer:

Fish Shell possui a função math para calcular datas no futuro ou passado. Basta usar o comando `math <expr>` e especificar a expressão para a data desejada. Por exemplo, para obter a data 5 dias no futuro a partir de hoje, podemos usar o seguinte código:

```
Fish Shell 
math now + 5 days
```

O resultado seria a data equivalente a 5 dias a partir de hoje. Outra funcionalidade útil é usar o `--date` flag para especificar uma data específica a partir da qual a operação será realizada. Por exemplo:

```
Fish Shell 
math --date "1 Jan 2021" + 2 weeks
```

Isso nos daria a data equivalente a duas semanas após a data especificada. Além disso, a função `duration` pode ser usada para definir um intervalo de tempo específico entre duas datas, permitindo que o usuário especifique uma data final ou um período de tempo a ser adicionado ou subtraído.

Deep Dive:

A capacidade de calcular datas no futuro ou passado é uma característica importante e amplamente utilizada em várias linguagens de programação. Ela permite que os programadores criem aplicativos mais dinâmicos e funcionais, ao mesmo tempo em que facilita o processo de agendamento de tarefas e definição de prazos. Alternativas para calcular datas incluem o uso de bibliotecas externas ou funções integradas em outras linguagens de programação. A implementação da função em Fish Shell é baseada em cálculos matemáticos e tratamento de datas em tempo unix.

Veja também:

- Documentação oficial do Fish Shell sobre o comando `math`: (https://fishshell.com/docs/current/cmds/math.html)
- Tutorial sobre como usar o comando `math` em Fish Shell: (https://dev.to/mfakhrusy/how-to-calculate-dates-with-fish-shell-2bbm)
- Exemplos e truques adicionais de como calcular datas usando Fish Shell: (https://medium.com/@juliengabryelewicz/bringing-concurrence-to-fish-shell-d0339b0d2def)