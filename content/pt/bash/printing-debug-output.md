---
title:                "Bash: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Imprimir saída de depuração é uma técnica muito útil para programadores que estão tentando descobrir erros em seus códigos Bash. Através da impressão de mensagens de depuração durante a execução do programa, é possível entender melhor o que está acontecendo com o código e identificar possíveis problemas.

## Como fazer:

Para imprimir uma mensagem de depuração em Bash, utilizamos o comando `echo`. O comando `echo` permite imprimir qualquer texto na tela. Podemos utilizá-lo com a opção `-e` para habilitar a interpretação de caracteres de escape, como por exemplo, a quebra de linha `\n`.

Podemos ainda utilizar o comando `printf`, que permite formatar a saída de acordo com parâmetros fornecidos. Por exemplo:

```Bash
echo -e "Iniciando execução do programa...\n"

# Exemplo de uso do comando printf
mensagem="Total de usuários cadastrados: %d\n"
usuarios=10
printf "$mensagem" $usuarios
```

A saída deste código seria:

```
Iniciando execução do programa...

Total de usuários cadastrados: 10
```

## Mergulho Profundo:

Além de simplesmente imprimir mensagens de texto, é possível utilizar outras ferramentas e técnicas para depurar um código em Bash. Por exemplo, podemos utilizar o comando `set -x` para habilitar a exibição dos comandos executados pelo programa, permitindo visualizar em detalhes como o código está sendo executado e identificar possíveis erros.

Também é possível redirecionar a saída de depuração para um arquivo, utilizando o operador `>>`. Isso permite que possamos revisar a saída de depuração mais tarde, caso necessário.

## Veja também:

- [Documentação do Bash](https://www.gnu.org/software/bash/manual/)
- [Como utilizar o comando echo no Bash](https://www.hostinger.com.br/tutoriais/comando-echo-linux/)
- [Debugando um código em Bash](https://www.cyberciti.biz/faq/debugging-bash-shell-script/)