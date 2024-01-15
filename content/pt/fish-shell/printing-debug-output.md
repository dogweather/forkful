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

## Por que imprimir saída de depuração?

Você está programando em Fish Shell e provavelmente se deparou com a necessidade de depurar seu código em algum momento. Imprimir saída de depuração é uma maneira rápida e eficaz de identificar e corrigir erros em seu código.

## Como fazer

O Fish Shell possui uma função integrada chamada `echo` que pode ser utilizada para imprimir saída de depuração. Veja um exemplo abaixo:

```
Fish Shell 3.1.2
~> set variavel "Olá, mundo!"

# Imprime a variável "variavel"
~> echo $variavel

Olá, mundo!
```

Você também pode usar a opção `-n` junto com o `echo` para evitar a quebra de linha ao final da saída. Isso pode ser útil em certos casos, como quando você precisa imprimir várias linhas de saída em uma única linha. Veja um exemplo:

```
Fish Shell 3.1.2
~> set linha1 "Isso é uma"
~> set linha2 "única linha"

# Imprime as duas variáveis em uma única linha
~> echo -n $linha1 $linha2

Isso é uma única linha
```

## Mais detalhes sobre a impressão de saída de depuração

Ao imprimir saída de depuração, é importante ter em mente que ela deve ser usada apenas para fins de depuração e não deve ser incluída no código final. Isso garantirá que seu código permaneça limpo e conciso.

Além disso, é possível redirecionar saídas de depuração para um arquivo de log, utilizando o operador `>` ou `>>`. Isso pode ser útil quando se depura um código em uma aplicação que não possui suporte para a exibição de saída de depuração.

## Veja também

- Documentação oficial da Fish Shell para a função `echo`: https://fishshell.com/docs/current/commands.html#echo
- Artigo sobre técnicas de depuração: https://medium.com/@isaaclyman/debugging-in-the-fish-shell-c0f4e8dd3219