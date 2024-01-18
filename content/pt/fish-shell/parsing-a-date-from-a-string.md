---
title:                "Analisando uma data de uma string."
html_title:           "Fish Shell: Analisando uma data de uma string."
simple_title:         "Analisando uma data de uma string."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que?

"Parsear" uma data a partir de uma string significa separar e converter uma sequência de texto que representa uma data em sua representação numérica equivalente. Os programadores realizam esse processo para facilitar a manipulação e cálculo de datas em seus códigos.

## Como fazer:

```
Fish Shell date -f %Y-%m-%d "2021-05-21"
```

Resultado: `1621562400`

O comando acima utiliza a função `date` do Fish Shell para converter a string "2021-05-21" em seu equivalente numérico, que representa a data em que este artigo foi escrito.

## Mais detalhes:

### Contexto Histórico:

A conversão de datas em representações numéricas é uma tarefa comum na programação, e sua complexidade depende muito do formato da data. Antes do Fish Shell, os programadores utilizavam outras ferramentas para realizar esse processo, como o comando `date` do Bash Shell.

### Alternativas:

Além do `date`, existem outras ferramentas e linguagens de programação que oferecem funções para parsear datas, como o JavaScript com a função `Date.parse()` e o Python com o módulo `datetime`. No entanto, o Fish Shell oferece uma sintaxe mais simples e intuitiva para realizar essa tarefa.

### Detalhes de implementação:

O Fish Shell utiliza a biblioteca C `strptime` para realizar o parsing de datas a partir de strings. Essa biblioteca é capaz de reconhecer uma grande variedade de formatos de datas e convertê-los em valores numéricos correspondentes.

## Veja também:

- Documentação sobre a função `date` do Fish Shell: https://fishshell.com/docs/current/commands.html#date
- Documentação da biblioteca C `strptime`: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strptime-strptime-l-strptime-l-wcsptime-wcsptime-l-mbstime-mbstime-l?view=msvc-160