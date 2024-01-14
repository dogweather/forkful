---
title:    "Fish Shell: Maiúsculas em uma string"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que usar o Fish Shell para capitalizar strings?

Se você está procurando uma forma simples e eficiente de capitalizar strings em seus scripts de programação, o Fish Shell pode ser a solução perfeita! Com sua sintaxe intuitiva e poderosas ferramentas de manipulação de strings, o Fish Shell torna o processo de capitalização de strings rápido e fácil.

## Como capitalizar strings usando o Fish Shell

Para capitalizar uma string usando o Fish Shell, basta utilizar o comando `string capitalize` seguido da string que você deseja capitalizar. Veja um exemplo:

```
Fish Shell> string capitalize "este é um exemplo"
Este é um exemplo
```

Além disso, você também pode utilizar o operador `|` para encadear comandos e capitalizar strings em um único comando:

```
Fish Shell> set my_string "este é um exemplo" | string capitalize
Este é um exemplo
```

## Deep Dive: Mais informações sobre a capitalização de strings

O comando `string capitalize` do Fish Shell utiliza a função `string capitalize` do próprio shell, que possui dois argumentos: a string a ser capitalizada e um valor booleano opcional para indicar se o restante da string deve ser convertido para letras minúsculas.

Além disso, o Fish Shell também oferece outras ferramentas de manipulação de strings, como `string lower` e `string upper`, que permitem converter strings para letras minúsculas e maiúsculas, respectivamente.

Com esses recursos, é possível criar scripts mais robustos e eficientes em que a capitalização de strings é necessária.

# Veja também

- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/
- Comunidade Fish Shell no Reddit: https://www.reddit.com/r/fishshell/
- Exemplos práticos de utilização da manipulação de strings no Fish Shell: https://fishshell.com/docs/current/cmds/string.html