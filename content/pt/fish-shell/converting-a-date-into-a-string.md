---
title:                "Convertendo uma data em uma string"
aliases:
- pt/fish-shell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:24.416687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data em uma string é transformar a representação de tempo (normalmente números) em texto legível. Fazemos isso para facilitar a leitura, armazenamento, ou uso em interfaces com usuários e outras funções dentro de programas.

## Como Fazer:

Aqui estão alguns exemplos de como converter datas em strings no Fish Shell:

```Fish Shell
set my_date (date "+%Y-%m-%d")
echo $my_date
```

Saída de exemplo:
```
2023-04-02
```

Também é possível formatar a data e a hora:

```Fish Shell
set my_datetime (date "+%Y-%m-%d %H:%M:%S")
echo $my_datetime
```

Saída de exemplo:
```
2023-04-02 15:45:30
```

## Mergulho Profundo:

O comando `date` existe em sistemas Unix desde os primeiros dias e é uma ferramenta padrão para trabalhar com tempo e datas. No Fish Shell, é comum invocar comandos Unix e usar as suas saídas. Ao contrário de outras shells que podem ter construções próprias para lidar com datas, Fish prefere utilizar ferramentas externas.

Alternativas ao comando `date` incluem scripts em Perl ou Python, que permitem uma manipulação mais complexa de datas. No entanto, para a maioria das tarefas simples, o comando `date` suficiente e mais eficiente.

Um detalhe importante é que o formato da string de data pode variar dependendo do sistema operacional ou localização geográfica (por exemplo, o formato americano MM/DD/YYYY versus o formato europeu DD/MM/YYYY), então é importante estar ciente disso ao trabalhar com strings de datas.

## Veja Também:

- Documentação oficial do comando `date` para verificar todos os formatos de string: [man7.org](https://man7.org/linux/man-pages/man1/date.1.html)
- Tutorial de Fish Shell para entender melhor os comandos e a sintaxe: [fishshell.com/docs](https://fishshell.com/docs/current/index.html)
