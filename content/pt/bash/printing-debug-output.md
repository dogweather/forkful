---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Depuração no Bash: Seu melhor amigo nos scripts

## O que é & Por quê?

Depuração, ou debug, é a técnica que usamos para inspecionar e entender o que está acontecendo dentro de um programa. Isso nos permite localizar e corrigir erros, monitorar o desempenho e entender melhor o fluxo de execução do nosso script.

## Como Fazer:

Podemos usar o comando `echo` para exibir mensagens de depuração no console:

```Bash
#!/bin/bash

echo "Iniciando o script..."
```

Para controlar melhor quando ver as mensagens de depuração, você pode usar uma variável:

```Bash
#!/bin/bash

DEBUG=true

if $DEBUG; then
  echo "Iniciando o script..."
fi
```

E executando este script, vamos ter:

```Bash
Iniciando o script...
```

## Aprofundando

Historicamente, a depuração tem sido uma parte crucial do desenvolvimento de programas. Desde os dias dos enormes mainframes até os modernos microserviços, os desenvolvedores usam a depuração para entender exatamente o que está acontecendo nos programas.

Existem várias alternativas ao `echo` em Bash para a depuração. Uma delas é a opção `-x` que você pode adicionar ao comando de interpretação Bash. Outra é utilizar ferramentas de depuração como o `gdb` (GNU debugger).

Quanto à implementação, o `echo` é uma função interna do Bash, tornando-o leve e rápido. Ele simplesmente imprime sua entrada para `stdout`, o que normalmente significa o console de comando.

## Veja Também:

Para aprender mais sobre a depuração em Bash, confira essas fontes: