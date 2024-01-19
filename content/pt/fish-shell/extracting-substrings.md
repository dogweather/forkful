---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrair Substrings em Fish Shell

## O que & Por quê?
Extrair substrings é o ato de retirar uma parte específica de uma string. Programadores fazem isso para manipular e analisar dados levando em conta apenas as informações necessárias.

## Como fazer:
No Fish Shell, você pode extrair substrings usando a função `string sub`. Veja um exemplo:

```fish
string sub -s 3 -l 5 "Ola, mundo"
```

Esse código retorna `a, mu`, que começa na 3ª posição e tem um comprimento de 5 caracteres.

## Mergulho Profundo
A extração de substrings tem suas raízes na concepção inicial das linguagens de programação. Em Fish Shell, oferece uma alternativa às expressões regulares ou ao uso de `awk` e `sed`.

A função `string sub` em Fish foi implementada como uma maneira menos verbosa e mais intuitiva de lidar com strings. Pela escolha do início (-s) e do comprimento (-l), você consegue extrair precisamente a parte desejada da string.

## Ver Também
Para mais informações sobre a manipulação de strings no Fish Shell, confira os seguintes links:
1. [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/commands.html#string)
2. [Tutorial sobre a manipulação de strings no Fish](https://jorge.fbarr.net/2011/11/27/fish-shell-string-functions/)
3. [Exemplos de uso do comando `string sub`](https://www.baeldung.com/linux/fish-shell-string-operations)