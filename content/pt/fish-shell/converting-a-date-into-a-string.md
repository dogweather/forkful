---
title:                "Convertendo uma data em uma sequência de caracteres"
html_title:           "Fish Shell: Convertendo uma data em uma sequência de caracteres"
simple_title:         "Convertendo uma data em uma sequência de caracteres"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string? 

Muitas vezes, precisamos exibir informações de data em nossos scripts de shell. No entanto, os formatos de data padrão podem ser difíceis de ler e entender. Converter uma data em uma string personalizada pode tornar a informação mais amigável e legível para o usuário final.

## Como fazer?

Se você estiver utilizando o Fish Shell, existem algumas maneiras simples de converter uma data em uma string. Uma delas é usando o comando `date`, que pode ser usado para exibir a data atual em diferentes formatos. Por exemplo:

```Fish Shell
date +"%d/%m/%Y"
```
Isso vai imprimir a data atual em um formato mais legível, como 05/11/2021.

Você também pode usar variáveis para armazenar a data e, em seguida, convertê-la em uma string usando o comando `string`. Veja um exemplo:

```Fish Shell
set data (date +"%d/%m/%Y")
echo "Hoje é $data."
```
Isso irá imprimir "Hoje é 05/11/2021".

## Profundando um pouco mais

Existem muitas opções de formato disponíveis para o comando `date`, que você pode usar para personalizar a data da maneira que preferir. Por exemplo, `%d` representa o dia, `%m` o mês e `%Y` o ano. Além disso, você pode adicionar outros caracteres, como barras ou traços, para separar as informações de data.

Além disso, você pode usar a opção `-d` para especificar uma data específica, em vez da data atual, e, em seguida, convertê-la em uma string. Por exemplo:

```Fish Shell
date -d "2021-12-25" +"%d/%m/%Y"
```
Isso irá imprimir "25/12/2021".

## Veja também

- [Página oficial do Fish Shell](https://fishshell.com)
- [Documentação do comando `date`](https://fishshell.com/docs/current/index.html#date)