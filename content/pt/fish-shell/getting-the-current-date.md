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

## Por que?

Quando estamos programando em linguagens de script, muitas vezes precisamos obter informações sobre a data atual. Isso pode ser necessário para criar arquivos com nomes exclusivos, registrar timestamps em registros de log ou até mesmo como parte de uma mensagem de saída em nosso script. Felizmente, o Fish Shell facilita a obtenção da data atual com alguns comandos simples.

## Como fazer:

Para obter a data atual no Fish Shell, podemos usar o comando `date` seguido de uma string de formatação. Por exemplo, para obter a data no formato "dia/mês/ano" podemos usar o seguinte comando:

```Fish Shell
date +%d/%m/%Y
```

Isso irá retornar a data atual no seguinte formato:

```Shell
11/10/2021
```

Também podemos obter informações adicionais, como o dia da semana, a hora atual e até mesmo o fuso horário, usando códigos de formatação específicos. Por exemplo, para obter o dia da semana e a hora atual com o fuso horário, podemos usar o seguinte comando:

```Fish Shell
date +"%A às %H:%M %Z"
```

Isso irá retornar algo parecido com:

```Shell
Segunda-feira às 18:30 CEST
```

## Mergulho profundo:

O comando `date` é na verdade um utilitário Unix que é amplamente suportado por diferentes sistemas operacionais e shells. Isso significa que podemos usar a mesma sintaxe e códigos de formatação em outros shells, como o Bash ou Zsh. Além disso, também podemos usar o comando `man date` para obter mais informações sobre todas as opções de formatação disponíveis.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Documentação do comando Unix "date"](https://www.unix.com/man-page/posix/1p/date/)
- [Tutorial do Fish Shell no DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell-in-linux-and-os-x)