---
title:                "Lendo argumentos da linha de comando"
aliases: - /pt/fish-shell/reading-command-line-arguments.md
date:                  2024-01-20T17:56:04.604035-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Ler argumentos da linha de comandos é pegar as informações que um usuário digita quando executa seu script. Programadores fazem isso para permitir que os scripts sejam mais flexíveis e interativos.

## How to:
Ler argumentos em Fish não podia ser mais direto. Vamos usar o `$argv`, que é uma lista contendo os argumentos passados para o script. 

```Fish Shell
#!/usr/bin/env fish

# Exibe o primeiro argumento
echo "Primeiro argumento: "$argv[1]

# Conta todos os argumentos passados
echo "Número de argumentos: "(count $argv)
```

Saída de exemplo ao executar `./meuscript.peixe arg1 arg2 arg3`:

```
Primeiro argumento: arg1
Número de argumentos: 3
```

## Deep Dive
No mundo do shell, passar argumentos é tão antigo quanto os próprios shells. No Fish, `$argv` é onde esses argumentos residem. Diferente de outros shells, como Bash, onde você usa `$1`, `$2`, etc., Fish trata os argumentos como uma lista, mais fácil de iterar ou acessar diretamente.

Alternativas populares incluem o uso de `getopts` ou `argparse` para funções mais complexas de manipulação de argumentos. Com `argparse`, você pode, por exemplo, definir flags e valores padrões de forma mais estruturada.

Quanto à implementação, lembre-se de que, em Fish, não usamos `$` para atribuir valores a variáveis; mas quando queremos ler o valor da variável, aí sim usamos.

```Fish Shell
set -l local_var "Olá, Fish!"
echo $local_var # Saída: Olá, Fish!
```

Mas para acessar elementos da lista `$argv`, usamos o `$` tanto para leitura quanto para atribuição, já que estamos acessando um valor e não criando uma variável.

## See Also
- [Fish Documentation on Variables](https://fishshell.com/docs/current/#variables)
- [Fish Tutorial on Arguments](https://fishshell.com/docs/current/tutorial.html#tut_arguments)
- [Stack Overflow: How to Parse Arguments in Fish](https://stackoverflow.com/questions/tagged/fish?sort=votes&pageSize=50)
