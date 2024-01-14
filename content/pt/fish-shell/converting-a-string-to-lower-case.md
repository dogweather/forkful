---
title:    "Fish Shell: Convertendo uma string para minúsculas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que?

Se você é um iniciante em programação, pode parecer confuso por que alguém precisaria converter uma string para minúsculas. Mas para aqueles que trabalham com linguagens de programação, essa é uma etapa crucial para garantir que o código funcione corretamente.

## Como Fazer

```Fish Shell
set minha_string "Hello World"
string tolower $minha_string
```

**Saída: hello world**

Aqui, estamos definindo uma variável `minha_string` com o valor de "Hello World". Em seguida, usamos o comando `string tolower` para converter essa string para minúsculas. O resultado é a saída "hello world". Esse processo é útil ao lidar com entradas de usuário, pois elas podem ser sensíveis a maiúsculas e minúsculas.

Além do comando `string tolower`, o Fish Shell também possui o comando `string toupper` para converter uma string para maiúsculas. Isso pode ser útil em situações em que você precisa alterar o caso de uma string de forma rápida e eficiente.

## Profundidade

Existem várias razões pelas quais pode ser necessário converter uma string para minúsculas. Por exemplo, ao comparar duas strings, pode ser necessário convertê-las para o mesmo caso para garantir uma comparação precisa. Além disso, o uso de letras maiúsculas e minúsculas em variáveis pode causar erros em alguns sistemas operacionais, portanto, converter para minúsculas pode ajudar a evitar esses problemas.

## Veja Também

- Documentação oficial do Fish Shell: `https://fishshell.com/docs/current/cmds/string.html#string-tolower`
- Como converter uma string para maiúsculas em Fish Shell: `https://example.com/converter-string-uppercase-fish-shell`