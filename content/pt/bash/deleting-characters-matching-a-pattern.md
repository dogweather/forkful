---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo Caracteres Correspondentes a um Padrão em Bash

## O Que & Por quê?

A eliminação de caracteres correspondentes a um padrão é uma técnica usada para manipular strings. Programadores fazem isso para limpar dados, manipular entradas do usuário ou formatar output.

## Como fazer:

No Bash, o comando `tr` pode ser usado para executar esta ação. Aqui está um exemplo:

```Bash
echo "Ola, Mundo!" | tr -d '!'
```

Nesse caso, o comando `tr -d '!'` removerá qualquer ocorrência do caractere '!'. A saída será:

```Bash
Ola, Mundo
```

Outro exemplo:

```Bash
echo "1234567890" | tr -d '0-5'
```

Aqui, o comando `tr -d '0-5'` apagará qualquer número de 0 a 5, e a saída será:

```Bash
67890
```

## Mergulho Profundo 

`tr` é um comando que é usado desde os primeiros dias do Unix. É usado para tr - traduzir ou transpor caracteres. A opção `-d` vem de 'delete'.

Existem alternativas para `tr`, como `sed` e `awk`. `sed` é mais versátil, enquanto `awk` é mais poderoso para manipular dados.

Detalhes da implementação: o comando `tr` lê a entrada da stdin, traduz ou exclui caracteres e grava o resultado na stdout.

## Veja Também

- [Página man do comando tr](https://linux.die.net/man/1/tr)
- [Exemplos comandos tr, sed, e awk](https://www.geekhideout.com/urlcode.shtml)
- [Detalhes de implementação de tr](https://unix.stackexchange.com/questions/8750/what-does-the-tr-command-do)