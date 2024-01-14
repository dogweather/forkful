---
title:                "Fish Shell: Concatenando strings"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma técnica comum no desenvolvimento de scripts e programas. Ela permite unir duas ou mais strings em uma única string, o que é útil para criar mensagens personalizadas, construir URLs dinâmicas e muito mais.

## Como fazer

Para concatenar strings no Fish Shell, você pode usar o operador `string1 string2` ou a função `string join`.

```Fish Shell

# Usando o operador
set str1 "Hello"
set str2 "world"
set result $str1 $str2
echo $result # Exibe "Hello world"

# Usando a função
set strList ("hello" "world" "!")
set result (string join " " $strList)
echo $result # Exibe "Hello world !"

```

## Mergulho Profundo

Ao concatenar strings, é importante ter em mente que os espaços entre as strings serão mantidos. Se você quiser unir as strings sem espaços, pode usar a função `string replace` para remover os espaços.

Além disso, se as strings que você está concatenando contêm caracteres especiais, é recomendável utilizar a função `string escape`. Isso garante que os caracteres especiais sejam tratados corretamente e não causem problemas no resultado final.

## Veja também

- [Documentação oficial do Fish Shell sobre concatenação de strings](https://fishshell.com/docs/current/cmds/string.html)
- [Guia de referência rápida do Fish Shell](https://fishshell.com/docs/current/cmds/index.html)
- [Tutorial interativo sobre concatenação de strings no Fish Shell](https://fishshell.com/docs/current/tutorial.html#string-concatenation