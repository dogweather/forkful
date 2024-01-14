---
title:    "Fish Shell: Convertendo uma data em uma sequência de caracteres"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente, provavelmente já se deparou com a necessidade de converter uma data em uma string. Isso pode ser útil para apresentar a data de forma mais legível para usuários ou para gerar relatórios com informações de data.

## Como converter uma data em string usando o Fish Shell

Para converter uma data em string com o Fish Shell, é preciso usar o comando `date` combinado com o operador `|%Y-%m-%d` para formatar a data no padrão desejado. Veja um exemplo abaixo:

```Fish Shell
set data (date +%Y-%m-%d)
```
O comando acima irá armazenar a data atual em uma variável chamada `data` no formato `aaaa-mm-dd`. É importante utilizar o operador `+%Y-%m-%d` para garantir que a data seja formatada corretamente.

Para visualizar a data armazenada na variável, basta utilizar o comando `echo` da seguinte forma:

```Fish Shell
echo $data
```
A saída será algo como: `2022-01-01`.

## Aprofundando um pouco mais

Além de converter para o formato padrão `aaaa-mm-dd`, é possível customizar a conversão para outros formatos, como por exemplo, utilizando o comando `strftime` para formatar a data de acordo com a linguagem do sistema.

Veja um exemplo abaixo:

```Fish Shell
set data (date -f "%A, %d de %B de %Y")
```
Nesse caso, a data será formatada em português, exibindo informações como o dia da semana, dia do mês e mês por extenso. A saída pode ser algo como: `Sábado, 01 de Janeiro de 2022`.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sobre o uso do comando `date` no Fish Shell](https://www.digitalocean.com/community/tutorials/como-usar-o-comando-date-no-fish-shell-pt)
- [Mais informações sobre o uso do comando `strftime`](https://www.man7.org/linux/man-pages/man3/strftime.3.html)