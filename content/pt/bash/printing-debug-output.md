---
title:                "Impressão de saída de depuração"
html_title:           "Bash: Impressão de saída de depuração"
simple_title:         "Impressão de saída de depuração"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Imprimir saída de depuração é uma técnica usada por programadores para identificar e corrigir erros em seu código. Isso envolve adicionar comandos de impressão na saída do programa durante o processo de desenvolvimento, para que os programadores possam rastrear o fluxo do código e identificar possíveis problemas.

## Como fazer:

Para imprimir saída de depuração em Bash, use o comando `echo` seguido da mensagem que deseja imprimir. Por exemplo, se você estiver tentando descobrir por que uma variável não está sendo definida corretamente, pode adicionar o comando `echo` seguido do nome da variável para imprimir seu valor atual.

```
nome_do_usuario="João"
echo $nome_do_usuario
```

Isso irá imprimir `João` na saída do programa, permitindo que você verifique se o valor foi definido corretamente.

## Detalhes extras:

Enquanto imprimir saída de depuração pode ser uma ferramenta útil para descobrir erros em seu código, é importante ter cuidado para não sobrecarregar o programa com muitos comandos de impressão. Além disso, existem outras técnicas de depuração, como usar um depurador, que podem ser mais eficazes em determinadas situações.

Como o Bash é uma linguagem de script, as instruções de impressão de saída também podem ser adicionadas em loops ou estruturas condicionais para verificar o fluxo do programa em diferentes caminhos.

## Veja também:

- [Tutorial de Depuração em Bash](https://www.shellscript.sh/debugging.html)
- [Documentação Oficial do Bash](https://www.gnu.org/software/bash/)