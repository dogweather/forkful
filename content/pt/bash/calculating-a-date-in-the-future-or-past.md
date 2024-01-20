---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Bash: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculando datas no passado ou futuro com Bash 

## O Que & Porquê?

Calcular uma data no passado ou futuro é a habilidade de determinar qual data será ou foi numa quantidade específica de dias, meses ou anos. Programadores fazem isso para manipular e formatar datas de acordo com as necessidades do projeto. 

## Como Fazer:

Vamos utilizar o comando `date` no Bash para calcular datas. O exemplo a seguir adiciona 5 dias à data atual:

```Bash
date -d "+5 days"
```

Para subtrair 5 dias da data atual, usamos:

```Bash
date -d "-5 days"
```

A saída será algo como:

```Bash
Seg Abr  5 13:00:00 BRT 2022
```

## Aprofundamento:

Historicamente, lidar com datas no passado ou futuro no Bash não era tão simplificado. O comando `date` não tem uma longa história de deslocamentos dinâmicos de data. Graças aos avanços no GNU Coreutils, tais cálculos estão mais acessíveis.

Existem alternativas ao comando `date`, como por exemplo o pacote `datetime` para Python e o módulo `Moment.js` para JavaScript. Eles oferecem capacidades semelhantes, mas a simplicidade e facilidade do comando `date` no Bash são incomparáveis para operações rápidas e diretas.

O comando `date -d` usa internamente um analisador para converter a expressão de texto passada como argumento em uma estrutura de data, fazendo os cálculos necessários para adicionar ou subtrair dias.

## Veja Também:

1. `man date`: A página do manual do comando `date` oferece informações completas sobre a sintaxe e suas opções.
   
2. [TimeAndDate.com](https://www.timeanddate.com): Um recurso online para explorar e entender melhor conceitos relacionados a datas e horas.
   
3. [GNU Coreutils](https://www.gnu.org/software/coreutils/coreutils.html): Para uma visão mais ampla das utilidades do Coreutils, incluindo o comando `date`.