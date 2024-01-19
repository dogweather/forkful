---
title:                "Capitalizando uma string"
html_title:           "Fish Shell: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizando Strings no Fish Shell

## O Que e Por Que?

Capitalizar uma string significa transformar a primeira letra de uma palavra em maiúscula. Os programadores fazem isso principalmente para melhorar a legibilidade e aparência dos textos dentro dos programas.

## Como Fazer:

Em Fish Shell, você pode capitalizar uma string usando a função `string upper`. Aqui está um exemplo rápido:

```Fish Shell
set nome "marketing"
echo (string upper $nome)
```

A saída deste exemplo seria:

```Fish Shell
MARKETING
```

## Mergulho Profundo:

Historicamente, a capitalização de strings foi introduzida em linguagens cedo como uma maneira de destaque na saída, particularmente em interfaces de usuário.

No Fish Shell, em comparação com outros shells UNIX, a capitalização de strings é muito fácil, graças ao conjunto de comandos `string`. Alternativamente, se a sua versão do Fish Shell não suportar a função `string upper`, você pode usar o comando `awk` como este:

```Fish Shell
echo "marketing" | awk '{ print toupper($1) }'
```

A implementação da função `string upper` no Fish Shell é simples e direta: ele simplesamente analisa cada caractere da string e o converte para maiúscula.

## Veja Também:

- Documentação oficial da função `string`: [https://fishshell.com/docs/3.1/commands.html#string](https://fishshell.com/docs/3.1/commands.html#string)
- Discussão sobre a função `string upper` no StackOverflow: [https://stackoverflow.com/questions/2264428/convert-strings-to-uppercase-in-fish-shell](https://stackoverflow.com/questions/2264428/convert-strings-to-uppercase-in-fish-shell)