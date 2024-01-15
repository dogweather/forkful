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

## Por que

Se você já se perguntou como descobrir o que seu código Bash está realmente fazendo, a impressão de saída de depuração pode ser a sua resposta. Ao imprimir saída de depuração, você pode ver exatamente os valores de suas variáveis e o processo do seu código, tornando a solução de erros e aprimoramento do seu código muito mais fácil.

## Como Fazer

Para imprimir saída de depuração em seu código Bash, você pode usar o comando "echo" seguido pela variável que deseja imprimir. Por exemplo:

```Bash
# Criando uma variável
nome="Maria"

# Imprimindo a variável de depuração
echo "O nome é: $nome"
```

Isso irá imprimir "O nome é: Maria" na sua saída, permitindo que você veja o valor da variável "nome" e entenda o que está acontecendo em seu código.

Você também pode usar o comando "set -x" para ativar a impressão de saída de depuração em todo o seu script. Por exemplo:

```Bash
#!/bin/bash

# Ativando impressão de saída de depuração
set -x

# Seu código aqui
```

Cada linha do seu código será impressa na saída, facilitando a visualização do processo do seu script.

## Mergulho Profundo

Além do comando "echo" e "set -x", existem outras ferramentas que você pode usar para imprimir saída de depuração em Bash. Por exemplo, o comando "printf" pode ser usado para formatar a saída para uma melhor legibilidade.

Você também pode redirecionar a saída de depuração para um arquivo de log, usando o operador ">>" seguido pelo nome do arquivo que deseja usar. Isso é útil quando você precisa analisar a saída de depuração mais tarde ou compartilhá-la com outros desenvolvedores para solucionar problemas.

## Veja Também

- [Bash Debugging Techniques](https://dev.to/freaksauce/bash-debugging-techniques-1h8h)
- [Mastering Bash debug output](https://www.linuxjournal.com/content/mastering-bash-debug-output)