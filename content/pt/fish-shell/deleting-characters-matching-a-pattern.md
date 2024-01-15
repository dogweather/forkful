---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Fish Shell: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Imagine a situação em que você está trabalhando no seu terminal e de repente percebe que digitou uma palavra com um ou mais caracteres extras no final. Para quem preza pela organização e precisão, isso pode ser frustrante. Felizmente, o Fish Shell oferece uma maneira rápida e eficiente de excluir caracteres correspondentes a um determinado padrão, economizando tempo e esforço.

## Como Fazer

```Fish Shell
# Para excluir todos os caracteres iguais ao padrão "!" no final de uma palavra
> echo meu_comando | tr -d '\n!'

# Para excluir todos os caracteres comuns a "123" no início de uma palavra
> echo meu_comando | tr -d '123'
```

O código acima usa o comando "tr" para excluir os caracteres que correspondem ao padrão especificado no final ou no início de uma palavra. Você pode personalizar o padrão para atender às suas necessidades, como remover todas as vogais ou símbolos específicos.

## Mergulho Profundo

O "tr" é um comando muito versátil que pode ser usado para várias tarefas, incluindo a exclusão de caracteres que correspondem a um padrão. Ele pode ser usado para substituir caracteres, deletar linhas vazias e muito mais. Ao aprender a usar esse comando, você estará expandindo seu conhecimento sobre o Fish Shell e suas capacidades.

# Veja Também

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Introduction to Fish Shell: A Beginner's Guide](https://www.baeldung.com/linux/fish-shell-intro)
- [10 Fish Shell Tips and Tricks You Should Know](https://www.makeuseof.com/tag/fish-shell-tricks-tips/)