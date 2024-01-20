---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Conversão de data para string com o Fish Shell

## O que e por que?

A conversão de data para string refere-se ao processo de transformar um objeto de data em um formato em texto. Programadores geralmente fazem isso para tornar os dados de data mais legíveis, ou para armazenar data de forma eficiente em um banco de dados.

## Como fazer: 

```fish
# obter a data e hora atual
set date (date)

# imprimir a data e hora
echo $date
```

No Fish Shell, a utilização da função `date` sem argumentos retorna a data e hora atual. A saída padrão é algo como:

```fish
Sun Sep 26 14:48:34 BST 2021
```

Para converter em uma string de data no formato desejado, você pode adicionar diversos argumentos ao comando `date`. Por exemplo, para obter a data no formato DD/MM/AAAA:

```fish
# obter a data no formato desejado
set date (date "+%d/%m/%Y")

# imprimir a data obtida
echo $date
```

O comando acima retorna uma saída semelhante a:

```fish
26/09/2021
```

## Mergulho profundo

Historicamente, a formatação da data era uma tarefa mais complexa e envolvia mais etapas. No entanto, Fish Shell simplificou esse processo, criando uma função integrada e fácil de usar, a `date`.

Além disso, existem outras alternativas para converter datas em strings, como a utilização de bibliotecas PHP ou JavaScript. No entanto, é importante notar que essas bibliotecas muitas vezes requerem importação e configuração, enquanto a função `date` do Fish Shell é uma utilidade incorporada e não requer nenhuma importação adicional.

Em relação aos detalhes de implementação, a função `date` no Fish Shell é uma implementação de um utilitário Unix de mesmo nome, que tem sido usado por décadas para manipulação de datas. A implementação no Fish Shell segue de perto o comportamento do comando Unix, com algumas melhorias e modificações para tornar o comando mais amigável e fácil de usar.

## Veja também

Para informações adicionais e análises mais detalhadas sobre a função `date`, consulte os seguintes links:

1. [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
2. [Referência do comando Unix 'date'](https://www.unix.com/man-page/posix/date)
3. [Tutorial do Linuxize sobre o comando 'date'](https://linuxize.com/post/linux-date-command/)