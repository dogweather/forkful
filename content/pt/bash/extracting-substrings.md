---
title:                "Extraindo subcadeias de caracteres"
html_title:           "Bash: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar extrair partes específicas de uma string em um script Bash para realizar uma determinada tarefa. Isso pode ser feito através da extração de substrings. Então, se você quer aprender como fazer isso, continue lendo!

## Como fazer

Usando o comando `cut` do Bash, podemos extrair substrings de uma string maior. Por exemplo:

```Bash
string="Este texto é um exemplo"
echo ${string:7} # irá imprimir "texto é um exemplo"
echo ${string:0:4} # irá imprimir "Este"
```

Para extrair uma substring do início até um certo caractere, podemos usar o seguinte formato `${string:start:length}`. Já para extrair uma substring a partir de um certo caractere até o final, o formato utilizado é `${string:start}`. Lembre-se que no Bash, os índices começam em 0.

## Mergulho Profundo

Outra forma de extrair substrings é usando o comando `grep`. Por exemplo, se quisermos extrair apenas as linhas que contêm a palavra "exemplo" em um arquivo de texto, podemos usar o seguinte comando:

```Bash
grep "exemplo" arquivo.txt
```

Também é possível restringir a busca para apenas uma parte específica da linha, usando o formato `${string:start:length}` como no exemplo anterior com `cut`.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial sobre comandos básicos do Bash](https://www.digitalocean.com/community/tutorials/bash-commands-and-scripting-cheat-sheet-pt)