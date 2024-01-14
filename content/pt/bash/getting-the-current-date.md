---
title:                "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que Programar em Bash?

Programar em Bash pode ser uma tarefa intimidadora para alguns, mas é uma habilidade muito útil para qualquer programador. Uma das coisas mais comuns que você pode querer fazer é obter a data atual em seus scripts. Neste post, vamos aprender a fazer isso em Bash.

## Como Obter a Data Atual em Bash

Para começar, você precisa usar o comando `date` seguido de um formato específico para obter a data atual. Abaixo está um exemplo básico:

```Bash
date +%Y-%m-%d
```

Este comando irá retornar a data atual no formato AAAA-MM-DD, por exemplo: 2021-01-31.

Você também pode adicionar outros formatos para obter informações como a hora, dia da semana, fuso horário, entre outros. Aqui estão alguns exemplos adicionais:

```Bash
date +%H:%M:%S # retorna a hora atual no formato HH:MM:SS
date +%A # retorna o dia da semana por extenso (por exemplo: Domingo)
date +"%Y-%m-%d %H:%M:%S" # combina múltiplos formatos em uma única saída (por exemplo: 2021-01-31 18:30:15)
```

## Mergulho Profundo: Obtendo a Data Atual em Detalhes

Agora que você já sabe como obter a data atual em Bash, vamos dar um mergulho mais profundo para entender melhor como isso funciona.

Quando você executa o comando `date` sem nenhum formato, ele irá retornar a data e hora completa no formato padrão da sua máquina. Porém, quando você adiciona os formatos (representados pelo caractere `%`), o comando irá interpretá-los e retornar apenas as informações que você especificou.

Além disso, você também pode usar a opção `-d` para obter a data de um dia específico. Por exemplo:

```Bash
date -d "3 days ago" +%Y-%m-%d # retorna a data de 3 dias atrás no formato AAAA-MM-DD
date -d "next Wednesday" +"%A, %d de %B de %Y" # retorna o dia da semana, dia e mês por extenso (por exemplo: Quarta-feira, 03 de Fevereiro de 2021)
```

## Veja Também

- [Site Oficial do Bash](https://www.gnu.org/software/bash/)
- [Documentação do Comando Date](https://www.gnu.org/software/coreutils/date)
- [Tutorial de Bash para Iniciantes](https://www.hostinger.com.br/tutoriais/comando-linux-bash-para-iniciantes)

Agora que você sabe como obter a data atual em Bash, use essa habilidade para criar scripts poderosos e automatizar suas tarefas de forma eficiente. Não tenha medo de experimentar diferentes formatos e opções para obter informações específicas de data e hora. Continue aprendendo e aprimorando suas habilidades em Bash!