---
title:                "Fish Shell: Imprimindo saída de depuração"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que?

A impressão de saída de depuração é uma técnica fundamental para o desenvolvimento de software, pois permite que os programadores inspecionem o estado do programa em diferentes pontos de execução. Isso pode ser especialmente útil para identificar e corrigir erros.

## Como fazer

```Fish Shell
# Utilizar o comando echo para imprimir uma mensagem de depuração
echo "Executou até aqui!"

# Imprimir o conteúdo de uma variável
set nome "Maria"
echo "Olá, $nome! Seu saldo é de $saldo."
```

A impressão de saída de depuração pode ser feita usando o comando `echo`, que imprimirá a mensagem especificada entre aspas. Também é possível imprimir o valor de uma variável usando a sintaxe `$variavel`. Isso pode ser útil para verificar o valor de variáveis em diferentes etapas do código.

## Mergulho Profundo

Há muitas outras maneiras de imprimir saída de depuração no Fish Shell. O uso de `echo` é uma forma rápida e simples, mas também é possível formatar a saída com a função `printf`. Além disso, o Fish Shell oferece uma variedade de variáveis internas que podem ser usadas para imprimir informações úteis, como a hora atual ou o diretório atual do usuário.

Além disso, é possível redirecionar a saída de depuração para um arquivo usando o operador `>` seguido pelo nome do arquivo. Isso pode ser útil para salvar registros de saída de depuração para referência posterior.

## Veja também

- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de introdução ao Fish Shell](https://www.learnshell.org/en/Fish_Shell)
- [Guia de uso do comando `echo`](https://linuxize.com/post/bash-echo-command/)