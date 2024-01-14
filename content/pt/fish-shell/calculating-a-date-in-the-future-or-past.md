---
title:    "Fish Shell: Calculando uma data no futuro ou passado"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por que utilizar o Fish Shell para calcular datas?

Se você é um programador ou entusiasta de tecnologia, provavelmente já ouviu falar do Fish Shell como uma alternativa para o Bash. Mas você sabia que também é possível utilizar o Fish Shell para realizar cálculos de datas? Isso pode ser especialmente útil para tarefas como agendamento de tarefas ou criação de scripts automatizados. Neste artigo, vamos explorar como utilizar o Fish Shell para calcular datas no passado ou no futuro.

## Como fazer

O Fish Shell oferece uma maneira simples e eficiente de calcular datas utilizando o seu terminal. Para isso, podemos utilizar o comando `date` seguido de uma fórmula de cálculo. Veja o exemplo abaixo:

```Fish Shell
date "3 days ago"
```

Esse comando irá retornar a data de hoje, mas três dias atrás. Outros exemplos possíveis seriam `date "next week"` para obter a data correspondente a uma semana a partir de hoje ou `date "last month"` para a data de um mês atrás.

Além disso, também é possível combinar o comando `date` com outros comandos como `echo` ou `read` para criar scripts mais complexos que utilizam cálculos de datas. Por exemplo, podemos criar um script que pede ao usuário uma data e retorna o dia da semana correspondente utilizando o seguinte código:

```Fish Shell
echo "Digite uma data (ex: 2021-01-01): "
read data
date -d $data +%A
```

## Aprofundando-se no cálculo de datas

Quando falamos em cálculo de datas, é importante entender como o Fish Shell interpreta as fórmulas utilizadas. O comando `date` possui uma lista de opções como `years` (anos), `months` (meses) e `hours` (horas), que podem ser utilizadas para especificar a unidade de tempo a ser calculada. Além disso, também é possível utilizar operadores matemáticos como adição `+` e subtração `-` para criar cálculos mais avançados.

Por exemplo, podemos utilizar a seguinte fórmula para obter a data de dois anos e três meses atrás:

```Fish Shell
date "2 years 3 months ago"
```

O resultado seria a data correspondente a 27 meses atrás a partir da data de hoje.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de início rápido do Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Exemplos de scripts com Fish Shell](https://github.com/fish-shell/fish-shell/tree/master/scripts)
- [Explicações detalhadas sobre como utilizar o comando `date`](https://fishshell.com/docs/current/commands.html#section-date)

# Veja também