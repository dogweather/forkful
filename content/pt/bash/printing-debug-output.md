---
title:    "Bash: Imprimindo saída de depuração"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que imprimir saída de debug?
Imprimir saída de debug é uma ferramenta essencial para identificar e corrigir erros em seus programas Bash. Ao imprimir mensagens de depuração, você pode rastrear o fluxo de execução do seu código e identificar onde algo está dando errado.

## Como fazer
Para imprimir saída de debug em Bash, utilize o comando `echo` seguido da mensagem que deseja imprimir. Por exemplo:

```Bash
echo "Iniciando loop"
```

Isso irá imprimir a mensagem "Iniciando loop" no terminal. Para imprimir variáveis, basta utilizar a sintaxe `$variavel`, por exemplo:

```Bash
nome="João"
echo "O nome é $nome"
```

Isso irá imprimir a mensagem "O nome é João". Você também pode utilizar o comando `printf` para imprimir saída de debug, que oferece mais opções de formatação. Por exemplo:

```Bash
idade=25
printf "Eu tenho %d anos" "$idade"
```

Isso irá imprimir a mensagem "Eu tenho 25 anos". Além disso, você pode utilizar o comando `set -x` no início do seu código para que todas as linhas sejam impressas como saída de debug.

## Mergulho profundo
Além de imprimir mensagens de depuração, você também pode utilizar outras técnicas para tornar a depuração mais eficaz. Por exemplo, você pode utilizar o comando `set -e` para que seu programa pare automaticamente caso encontre algum erro, o que pode ajudar a identificar o exato ponto onde o problema está ocorrendo.

Também é possível utilizar o comando `trap` para capturar sinais e imprimir mensagens de erro específicas, facilitando o entendimento do processo de depuração.

## Veja também
- [Depuração com Bash](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Debugging-Bash)
- [Saída de depuração em Bash](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html)
- [Comandos de controle de execução do Bash](https://tiswww.case.edu/php/chet/bash/bashref.html#Using-Set)