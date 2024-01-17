---
title:                "Convertendo uma data em uma string"
html_title:           "Bash: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma data em uma string significa transformar uma data em formato legível em uma sequência de caracteres. Os programadores geralmente fazem isso para facilitar a manipulação e exibição de datas em seus programas.

## Como fazer:

Para converter uma data em uma string no Bash, podemos usar o comando```date```. Veja um exemplo:

```
#!/bin/bash

data_atual=$(date +"%d/%m/%Y")

echo "A data de hoje é: $data_atual"
```

A saída desse código seria algo como: "A data de hoje é: 21/05/2021", atualizada para a data atual.

## Mais detalhes:

- Contexto histórico: o Bash é um interpretador de comandos para sistemas operacionais baseados em Unix, lançado pela primeira vez em 1989.
- Alternativas: existem outras formas de converter datas em strings, como usar bibliotecas de programação específicas ou comandos de outros interpretadores de comandos, como o PowerShell do Windows.
- Detalhes da implementação: a função ```date``` do Bash possui uma grande quantidade de opções de formatação, que podem ser consultadas no manual do terminal.

## Veja também:

Para mais informações sobre o uso do comando ```date``` no Bash, consulte o manual do terminal usando o comando ```man date```.