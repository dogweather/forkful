---
title:                "Imprimindo saída de depuração"
html_title:           "Fish Shell: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e por que?

Debug é uma técnica usada por programadores para verificar e corrigir erros em seus códigos. Ao imprimir saídas de debug, os programadores podem visualizar informações importantes sobre variáveis, fluxo de execução e lógica de seus códigos. Isso permite uma depuração mais eficiente e eficaz do código.

## Como fazer:

O Fish Shell oferece uma variedade de maneiras de imprimir saídas de debug. Aqui estão alguns exemplos:

Dentro do código Fish Shell `` ` `
`` `fish
set variavel "valor"
debug variavel
`` `
Saída:
Variável: valor

É possível imprimir informações adicionais usando o comando `echo` seguido de uma variável, por exemplo:

Dentro do código Fish Shell `` ` `
`` `fish
set var1 "Hello "
set var2 "World"
echo $var1$var2
`` `
Saída:
Olá Mundo

Também é possível imprimir o valor de uma variável em um formato específico, como `tabela` ou `json`, usando o sinal de igual (=) após o nome da variável seguido do formato desejado:

Dentro do código Fish Shell `` ` `
`` `fish
set variavel "valor"
debug variavel = tabela
`` `
Saída:
Variável = (valor)

## Mergulho profundo:

A impressão de saída de debug é uma técnica amplamente utilizada em programação, e remonta aos primeiros dias da computação. Antes dos computadores modernos e das ferramentas avançadas de depuração, os programadores costumavam imprimir mensagens de texto simples como saída de debug para detectar erros em seus códigos.

Além disso, existem outras alternativas para imprimir saídas de debug, como o uso de um depurador, que permite que os programadores acompanhem seu código passo a passo e visualizem informações sobre suas variáveis em tempo real.

O Fish Shell também oferece a opção de desativar a impressão de saída de debug usando o comando `set -q DEBUG_OUTPUT` e definindo o valor para `0`. Isso pode ser útil para remover a desordem de saída de debug em código mais complexo.

## Veja também:

Para saber mais sobre como usar e aproveitar ao máximo as funcionalidades de debug do Fish Shell, confira a documentação oficial em https://fishshell.com/docs/current/commands.html#printing-debug-output.