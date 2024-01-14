---
title:                "Bash: Obtendo a data atual"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que usar o comando de data em Bash?

O Bash é uma linguagem de script amplamente utilizada em sistemas operacionais Unix e Linux. Uma das funcionalidades mais úteis é o comando de data, que permite que o usuário obtenha a data e a hora atuais do sistema. Isso pode ser útil em vários cenários, como criar logs de eventos, agendar tarefas ou até mesmo para exibir a data atual em um prompt de comando personalizado.

## Como usar o comando de data em Bash

Para obter a data e a hora atuais em Bash, basta usar o comando "date":

```Bash
date
```

Isso irá imprimir a data e a hora atuais no formato padrão do sistema.

```
Wed Jun 23 12:47:23 CEST 2021
```

Você também pode formatar a saída da data de acordo com suas necessidades. Por exemplo, para obter apenas a data no formato "dd/mm/aaaa", você pode usar o seguinte comando:

```Bash
date "+%d/%m/%Y"
```

Isso irá imprimir apenas a data atual no formato especificado.

```
23/06/2021
```

Existem várias opções de formatação disponíveis para personalizar a saída do comando de data. Você pode verificar a documentação oficial do Bash para obter uma lista completa dessas opções.

## Mergulho profundo no comando de data em Bash

Por trás dos bastidores, o comando "date" em Bash usa a função "strftime" para formatar a data de acordo com as opções especificadas. A função "strftime" é uma função C que permite formatar datas e horas em uma variedade de formas. Ao usar o comando "date" em Bash, você está efetivamente usando essa função para obter a data atual.

Além disso, o comando de data também pode ser usado para definir a data e hora do sistema. Isso pode ser feito especificando a data e hora desejadas no seguinte formato: "MMDDhhmmYYYY.ss". Por exemplo, para definir a data como 23 de junho de 2021 às 12:30, você pode usar o seguinte comando:

```Bash
date 062312302021.00
```

Isso irá alterar a data atual para a que você especificou.

## Veja também

- Documentação oficial do Bash: https://www.gnu.org/software/bash/
- Documentação oficial do comando date: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Função strftime na linguagem C: https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm