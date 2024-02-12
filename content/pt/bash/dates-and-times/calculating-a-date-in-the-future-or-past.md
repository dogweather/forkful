---
title:                "Calculando uma data no futuro ou passado"
aliases:
- /pt/bash/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:30:45.944901-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Calcular datas no futuro ou no passado é simplesmente modificar uma data existente para descobrir qual data será ou foi. Programadores fazem isso para agendar tarefas, gerar lembretes ou validar prazos.

## Como Fazer:

```Bash
# Para adicionar dias a uma data
data_futura=$(date -d "2023-04-15 + 10 days" +%F)
echo $data_futura
# Saída esperada: 2023-04-25

# Para subtrair dias de uma data
data_passada=$(date -d "2023-04-15 - 10 days" +%F)
echo $data_passada
# Saída esperada: 2023-04-05

# Para adicionar ou subtrair outras unidades (meses, anos)
data_modificada=$(date -d "2023-04-15 + 1 month - 1 year" +%F)
echo $data_modificada
# Saída esperada: 2022-05-15
```

## Aprofundamento

A capacidade de manipular datas é essencial na automação e no gerenciamento de tarefas baseadas em tempo. Tradicionalmente, Unix e sistemas derivados oferecem a ferramenta `date` que permite a manipulação de datas de formas variadas.

Alternativas incluem comandos como `at` e `cron` para agendar tarefas baseadas em datas futuras calculadas, mas `date` é o utilitário mais flexível para cálculos rápidos. Quanto à implementação, o Bash utiliza internamente funções de C do sistema operacional para calcular as datas, sendo relativamente precisas e confiáveis.

Por fim, é importante entender as diferenças de fuso horário e como o verão (DST) pode afetar o cálculo de datas. Para programas mais complexos, ferramentas como `date` podem não ser suficientes, recomendando-se linguagens de programação com bibliotecas de gerenciamento de data e hora mais robustas.

## Veja Também

- Manual do Bash (`man bash`) para mais informações de scripting: https://www.gnu.org/software/bash/manual/
- Documentação da ferramenta `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Guia de agendamento de tarefas com `cron` e `at`: https://www.cyberciti.biz/faq/how-do-i-add-jobs-to-cron-under-linux-or-unix-oses/
- Artigo sobre manipulação de data e hora em scripts: https://www.tldp.org/LDP/abs/html/timedate.html
