---
title:    "Fish Shell: Obtendo a data atual"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar o Fish Shell para obter a data atual?

Se você é um programador que deseja automatizar tarefas diárias ou simplesmente deseja saber a data atual para fins de organização, usar o Fish Shell para obter a data atual pode ser muito útil. O Fish Shell é uma linguagem de script de shell interativa moderna e poderosa, conhecida por sua simplicidade e expressividade. Neste artigo, vamos mostrar como é fácil obter a data atual usando o Fish Shell.

## Como fazer:

Vamos ver como podemos usar o Fish Shell para obter a data atual com alguns exemplos práticos. Antes de começarmos, certifique-se de que você tem o Fish Shell instalado no seu computador.

```Fish Shell
$ date
02/06/2021
```

Este é um exemplo simples que usa o comando `date` para imprimir a data atual no formato DD/MM/YYYY. Mas e se quisermos alguma personalização na saída? Podemos usar a opção `-f` seguido de um formato de data específico:

```Fish Shell
$ date -f "Hoje é %d de %B de %Y"
Hoje é 02 de Junho de 2021
```

Aqui, imprimimos a data atual no formato "dia de mês de ano" e adicionamos algumas palavras extras para torná-lo mais legível.

## Aprofundando:

Você pode estar se perguntando como o Fish Shell obtém a data atual em primeiro lugar. Na verdade, o Fish Shell faz chamadas para a biblioteca C padrão `libc` para isso. Mas o Fish Shell também possui uma função interna chamada `date` que pode ser usada para obter a data atual. Podemos ver a definição dessa função usando o comando `functions`:

```Fish Shell
$ functions date
function date --description 'Print the date and time'
    builtin date $argv
end
```

Aqui vemos que a função `date` simplesmente usa a função de biblioteca `libc` do Fish Shell com alguns argumentos opcionais passados.

## Veja também:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de instalação do Fish Shell](https://fishshell.com/docs/current/tutorial.html#how-do-i-get-it)
- [Guia de uso básico do Fish Shell](https://fishshell.com/docs/current/tutorial.html#how-do-i-use-it)

## Veja também:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de instalação do Fish Shell](https://fishshell.com/docs/current/tutorial.html#how-do-i-get-it)
- [Guia de uso básico do Fish Shell](https://fishshell.com/docs/current/tutorial.html#how-do-i-use-it)