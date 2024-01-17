---
title:                "Obtendo a data atual"
html_title:           "Fish Shell: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por que fazer:
Pegar a data atual é um recurso muito útil para programadores, pois permite que eles obtenham a data e a hora atual em que o código está sendo executado. Isso pode ser usado para registrar eventos, criar nomes de arquivo com marcação de data e hora, ou para fins de depuração.

## Como fazer:
Para obter a data atual usando o Fish Shell, podemos usar o comando `date`. Por exemplo, para imprimir a data atual no formato ISO 8601, podemos usar o seguinte código:

```
Fish Shell  date +%Y-%m-%d
```

Isso irá imprimir a data atual no formato "ano-mês-dia", por exemplo, `2020-08-15`. Podemos também incluir a hora atual adicionando `%H:%M:%S` ao comando, resultando em `2020-08-15 12:30:00`.

## Mergulho profundo:
Em versões anteriores do Fish Shell, o recurso de pegar a data atual era limitado e incentivava o uso de outros comandos, como `date` ou `date -u`. No entanto, na versão mais recente do Fish Shell, esse recurso foi aprimorado, permitindo que os usuários obtenham facilmente a data e a hora atual sem a necessidade de comandos extras.

Existem também alternativas para obter a data e a hora atual, como usar a biblioteca `python-datetime` ou o módulo `moment.js` no JavaScript. No entanto, para simplificar e ter um código mais leve, a utilização do comando `date` no Fish Shell é a opção mais recomendada.

## Veja também:
- Documentação oficial do Fish Shell para o comando date: https://fishshell.com/docs/current/cmds/date.html
- Referência de formato de data e hora ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html