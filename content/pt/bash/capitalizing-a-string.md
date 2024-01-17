---
title:                "Transformando uma string em maiúsculas"
html_title:           "Bash: Transformando uma string em maiúsculas"
simple_title:         "Transformando uma string em maiúsculas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string em Bash significa transformar todas as letras em maiúsculas. Os programadores fazem isso para padronizar a formatação de saída ou para realizar comparações de strings sem se preocupar com a diferenciação entre letras maiúsculas e minúsculas.

## Como fazer:

Para capitalizar uma string em Bash, você pode usar o comando `tr` combinado com `echo`. Aqui está um exemplo simples:

```Bash
texto="exemplo de capitalização de string"
echo $texto | tr '[:lower:]' '[:upper:]'
```
A saída será "EXEMPLO DE CAPITALIZAÇÃO DE STRING".

## Explore mais:

- Na história do Bash, o uso de `tr` para capitalizar uma string remonta ao Unix nos anos 70, mas também existem outras maneiras de fazer isso, como o comando `awk`.
- Além de usar `tr` com o `echo`, você também pode usá-lo com entradas de arquivo ou comandos de substituição.
- Para capitalizar apenas a primeira letra de uma string, você pode usar o comando `sed` combinado com `tr`.

## Veja também:

- [Um guia rápido para comandos básicos do Bash](https://www.hostinger.com.br/tutoriais/comandos-linux-basicos)
- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial para iniciantes em Bash](https://www.devmedia.com.br/introducao-ao-shell-script-no-linux-linux/25778)