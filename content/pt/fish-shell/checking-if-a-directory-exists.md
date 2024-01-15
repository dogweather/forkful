---
title:                "Verificando se um diretório existe"
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Você pode querer verificar se um diretório existe antes de criar um novo diretório ou executar outras operações nele. Isso evita erros e problemas em seu código e economiza tempo, pois não é necessário lidar com erros desnecessários.

## Como fazer

Para verificar se um diretório existe em Fish Shell, você pode usar o comando `test -d` seguido pelo caminho do diretório que deseja verificar. Por exemplo:

```Fish Shell
test -d /Users/usuario/Documentos
```

Se o diretório existir, esse comando retornará um valor de "verdadeiro" (true). Caso contrário, retornará "falso" (false).

Você também pode usar o comando `if` para executar ações específicas com base no resultado da verificação. Por exemplo:

```Fish Shell
if test -d /Users/usuario/Downloads
    echo "O diretório Downloads existe!"
else
    echo "O diretório não existe. Criando agora..."
    mkdir /Users/usuario/Downloads
end
```

Isso irá imprimir uma mensagem diferente com base no resultado da verificação. Se o diretório existir, será exibida a mensagem "O diretório Downloads existe!" Caso contrário, será criado um novo diretório com o nome "Downloads".

## Mergulho profundo

O comando `test -d` faz parte dos comandos de teste no Fish Shell, que são usados ​​para avaliar valores e retornar "verdadeiro" ou "falso". Outros comandos de teste incluem `test -e` para verificar a existência de um arquivo e `test -f` para verificar se um arquivo é um arquivo regular.

Ao usar o comando `if`, você pode usar a opção `-s` para verificar se o tamanho de um arquivo/diretório é maior que zero. Por exemplo:

```Fish Shell
if test -s /Users/usuario/Documentos/meu_arquivo.txt
    rm /Users/usuario/Documentos/meu_arquivo.txt
else
    echo "O arquivo está vazio."
end
```

Isso irá excluir o arquivo apenas se ele tiver um tamanho maior que zero.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Comandos de teste no Fish Shell (em inglês)](https://fishshell.com/docs/current/index.html#conditional-execution)
- [Tutorial do Fish Shell no dev.to (em inglês)](https://dev.to/erik109/navigating-fish-shell-for-the-first-time-8o7/)