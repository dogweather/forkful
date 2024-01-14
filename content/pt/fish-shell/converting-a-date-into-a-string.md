---
title:    "Fish Shell: Converter uma data em uma string"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que usar a conversão de data para string?

A conversão de data para string é uma habilidade útil para programadores que trabalham com datas em seus projetos. Ao converter uma data em formato de string, é possível manipulá-la facilmente e utilizá-la em diferentes partes do código.

## Como fazer a conversão de data para string no Fish Shell
Para fazer a conversão de data para string no Fish Shell, é preciso utilizar o comando `strftime`. Ele permite especificar um formato de data e hora personalizado para a conversão. Vejamos um exemplo:

```Fish Shell
set data (date -f "%d/%m/%Y") #Atribui a data atual na variável "data"
echo $data #Imprime a data no formato "dd/mm/aaaa"
```
A saída desse exemplo seria: 01/06/2021.

## Profundando na conversão de data para string
O comando `strftime` também pode ser utilizado para formatar outros aspectos da data, como o dia da semana, o mês e o ano. É possível consultar a documentação do Fish Shell para ver todas as opções disponíveis. Além disso, é importante mencionar que, ao utilizar a conversão de data para string, deve-se levar em conta a localização do sistema, já que diferentes países possuem diferentes formatos de data. 

## Veja também
- Documentação do comando `strftime` no Fish Shell: https://fishshell.com/docs/current/commands.html#strftime
- Como manipular datas no Fish Shell: https://fishshell.com/docs/current/tutorial.html#dates
- Conversão de data para string em outros shells: https://www.computerhope.com/unix/strftime.htm