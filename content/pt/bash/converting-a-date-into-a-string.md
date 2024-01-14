---
title:    "Bash: Convertendo uma data em uma string"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Há muitas vezes em que precisamos manipular datas em nossos programas Bash. Por exemplo, podemos querer renomear um arquivo com a data em que foi criado ou inserir a data atual em um arquivo de log. Para fazer isso, precisamos converter a data em formato de string, tornando-o legível para nós e para outros programas.

## Como fazer

Existem algumas maneiras de converter uma data em uma string no Bash, mas a mais comum é usando o comando `date` em conjunto com o parâmetro `-I` para retornar a data em formato ISO 8601 (AAAA-MM-DD). Vamos ver um exemplo:

```Bash
data=$(date -I)
echo "Hoje é $data"
```

A saída será algo como:

`Hoje é 2021-08-25`

Podemos especificar o horário junto com a data, adicionando o parâmetro `-Iseconds` ao comando `date`. Por exemplo:

```Bash
data_hora=$(date -Iseconds)
```

A saída será algo como:

`2021-08-25T13:25:00+02:00`

## Mergulho profundo

Além do formato ISO 8601, o comando `date` também suporta uma ampla variedade de formatos. Podemos especificar o formato desejado usando o parâmetro `+` seguido de códigos de formato. Por exemplo, para retornar a data em formato "dia/mês/ano", podemos usar o parâmetro `+%d/%m/%Y`.

```Bash
data=$(date +%d/%m/%Y)
echo "Data atual: $data"
```

A saída será algo como:

`Data atual: 25/08/2021`

Você pode encontrar uma lista completa dos códigos de formato suportados no manual do comando `date` ou online.

## Veja também

- [Manual do comando date](https://man7.org/linux/man-pages/man1/date.1.html)
- [ISO 8601:2004 - Data e hora - Representação da data e da hora](https://www.iso.org/standard/40874.html)