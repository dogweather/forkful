---
title:                "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao escrever um programa em Fish Shell, é importante garantir que o código seja robusto e possa lidar com diferentes situações, como a possibilidade de um diretório não existir. Portanto, verificar se um diretório existe é uma boa prática para evitar erros e falhas no seu programa.

## Como fazer isso no Fish Shell

A seguir, mostrarei como você pode verificar se um diretório existe usando a sintaxe do Fish Shell e também como verificar o status de saída do comando para determinar se o diretório existe ou não.

```Fish Shell
if test -d /caminho/do/diretório
    echo "O diretório existe."
end
```

Neste exemplo, usamos o comando `test` com a opção `-d` para verificar se o caminho especificado é um diretório válido. Em seguida, usamos o comando `echo` para imprimir uma mensagem caso o diretório exista.

Você também pode usar o status de saída do comando `test` para realizar uma ação com base no resultado. Por exemplo:

```Fish Shell
if test -d /caminho/do/diretório
    # Executa um comando se o diretório existir
else
    # Executa outro comando se o diretório não existir
end
```

## Uma análise mais profunda

Além de verificar se um diretório existe, você também pode usar comandos do Fish Shell para realizar verificações mais específicas, como se o diretório está vazio ou tem permissões de escrita. Você também pode combinar a verificação de diretório com outros comandos e expressões, como a verificação de tamanho ou conteúdo de arquivos dentro do diretório.

Outro fator importante é o uso de variáveis para armazenar os caminhos dos diretórios e tornar seu programa mais flexível e dinâmico. Você pode usar a sintaxe `set` para criar e atribuir valores a variáveis no Fish Shell.

## Veja também

- Documentação oficial do Fish Shell (em inglês): https://fishshell.com/docs/current/
- Tutoriais de programação em Fish Shell (em inglês): https://edouardklein.com/fish-sh