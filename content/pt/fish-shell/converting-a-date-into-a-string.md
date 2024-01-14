---
title:                "Fish Shell: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que utilizar o Fish Shell para converter uma data em uma string?

Não importa se você é novo na programação ou se já tem anos de experiência, há momentos em que é necessário converter uma data em uma string para realizar determinadas tarefas. O Fish Shell torna esse processo simples e eficiente, permitindo que você se concentre em outras partes importantes do seu código.

## Como fazer a conversão de data em string utilizando o Fish Shell

Para converter uma data em uma string no Fish Shell, basta seguir alguns passos simples. Primeiro, é necessário ter uma data no formato `AAAA-MM-DD` salva em uma variável. Então, utilizando o comando `date` e especificando o formato desejado, podemos converter essa data em uma string. Veja um exemplo abaixo:

```Fish Shell
set data 1995-02-22
set data_string (date --date=$data '+Dia %d, mês %m, ano %Y')
echo $data_string # Output: Dia 22, mês 02, ano 1995
```

Com essa técnica, é possível personalizar a formatação da data de acordo com as suas necessidades. Você pode adicionar ou remover informações no comando `date` para obter a string desejada.

## Aprofundando-se na conversão de data em string no Fish Shell

O Fish Shell utiliza o comando `date` do sistema operacional para realizar a conversão de data em string. Isso significa que, ao utilizar esse comando, você tem acesso a diversos formatos e opções para personalizar a saída da string. É importante ressaltar que o formato escolhido pode variar de acordo com o sistema operacional utilizado.

Uma outra opção é utilizar a função `strftime`, que também permite a formatação personalizada da data. Além disso, você pode consultar a documentação do Fish Shell para descobrir mais informações e opções sobre a conversão de data em string.

## Veja também

- [Documentação do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de referência do comando date](https://linux.die.net/man/1/date)
- [Documentação strftime no Fish Shell](https://fishshell.com/docs/current/cmds/strftime.html)