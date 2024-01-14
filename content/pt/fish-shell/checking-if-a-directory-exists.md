---
title:                "Fish Shell: Verificando se um diretório existe"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você está tentando criar um script que precisa verificar se um diretório específico existe? Saber como fazer essa verificação pode economizar tempo e evitar erros em sua programação.

## Como Fazer

Para verificar se um diretório existe no Fish Shell, você pode usar o comando `test` combinado com o argumento `-d` seguido do nome do diretório que deseja verificar. Por exemplo:

```Fish Shell
test -d NomeDoDiretorio
```

Se o diretório existir, não haverá nenhuma saída no terminal. Se o diretório não existir, você verá um erro indicando que o diretório não foi encontrado.

## Deep Dive

Ao usar o comando `test` seguido do argumento `-d`, o Fish Shell está verificando se o arquivo passado como argumento é um diretório. Se o diretório for encontrado, o comando `test` retorna o valor "verdadeiro" (ou seja, 0) e, portanto, não há saída no terminal. Se o diretório não for encontrado, o comando retorna o valor "falso" (ou seja, 1) e, portanto, você vê o erro indicando que o diretório não foi encontrado.

Você também pode usar o mesmo método para verificar se um arquivo existe, usando o argumento `-f`. E para verificar se um link simbólico existe, use o argumento `-L`.

Se você precisar de uma verificação mais detalhada, o Fish Shell também oferece o comando `test` com vários outros argumentos, como `-w` para verificar se um diretório é gravável e `-x` para verificar se um arquivo é executável.

## Veja Também

- [Documentação do Fish Shell - Comando Test](https://fishshell.com/docs/current/cmds/test.html)
- [Tutorial do Fish Shell - Verificando Condições](https://fishshell.com/docs/current/tutorial.html#verificando-condicoes)
- [GitHub - Exemplos de Uso do Comando Test](https://github.com/fish-shell/fish-shell/blob/master/test/test_wiki.md)