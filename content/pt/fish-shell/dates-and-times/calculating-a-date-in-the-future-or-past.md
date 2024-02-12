---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:31:01.614346-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Calcular datas futuras ou passadas é o processo de adicionar ou subtrair dias a uma data específica. Programadores utilizam essa habilidade para automatizar lembretes, gerenciar prazos ou criar relatórios baseados em períodos de tempo.

## Como Fazer:
Calculando 5 dias no futuro:
```Fish Shell
set data_atual (date +%Y-%m-%d)
set data_futura (date -d "$data_atual + 5 days" +%Y-%m-%d)
echo $data_futura
```
Saída de exemplo:
```
2023-04-05
```

Calculando 10 dias no passado:
```Fish Shell
set data_atual (date +%Y-%m-%d)
set data_passada (date -d "$data_atual - 10 days" +%Y-%m-%d)
echo $data_passada
```
Saída de exemplo:
```
2023-03-21
```

## Mergulho Profundo
Calcular datas é uma funcionalidade essencial desde o início da programação. Antes de bibliotecas e funções dedicadas, cálculos de data eram realizados manualmente, o que era suscetível a erros. Hoje, Fish Shell, assim como outras shells e linguagens de programação, incorpora ferramentas que simplificam essas operações.

Outras ferramentas como `dateutils`, `GNU coreutils`, ou pacotes de linguagens de programação como `DateTime` em Python, substituíram métodos antigos.

Em Fish, a manipulação de datas se apoia principalmente no comando `date` Unix, que é robusto mas pode ter pequenas diferenças entre sistemas (*BSD vs GNU, por exemplo). Fish não tem recursos inerentes de manipulação de datas, então invoca ferramentas do sistema.

## Ver Também
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils - Date Input Formats](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)
- [Stack Overflow - Manipulating Dates in Fish Shell](https://stackoverflow.com/questions/tagged/date+fish)
