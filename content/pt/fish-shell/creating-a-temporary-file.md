---
title:    "Fish Shell: Criando um arquivo temporário"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário com Fish Shell?

Criar um arquivo temporário é uma tarefa comum ao desenvolver scripts de shell. Esses arquivos são úteis para armazenar informações temporárias ou para criar arquivos de teste durante o processo de desenvolvimento. O Fish Shell oferece uma maneira fácil e eficiente de criar e gerenciar esses arquivos temporários.

## Como criar um arquivo temporário com Fish Shell

Para criar um arquivo temporário com Fish Shell, use o comando `mktemp` seguido de um nome de arquivo opcional. Por exemplo, se quisermos criar um arquivo temporário chamado "temp.txt", podemos usar o seguinte comando:

```Fish Shell
mktemp temp.txt
```

Podemos então usar o arquivo temporário recém-criado para armazenar informações usando outros comandos do shell, como `echo` ou `cat`.

```Fish Shell
echo "Informações importantes" > temp.txt
cat temp.txt # imprime "Informações importantes"
```

Uma vez que tenhamos terminado de usar o arquivo temporário, podemos excluí-lo usando o comando `rm`.

```Fish Shell
rm temp.txt
```

## Aprofundando na criação de um arquivo temporário

Ao criar um arquivo temporário com Fish Shell, ele será armazenado em uma pasta padrão, geralmente `/tmp`. No entanto, é possível especificar um diretório diferente usando a opção `--tmpdir` no comando `mktemp`. Além disso, podemos especificar um modelo de nome de arquivo usando a opção `-t`, que pode ser útil se quisermos que o arquivo temporário tenha um nome específico ou siga um padrão.

Existem também várias opções adicionais disponíveis no comando `mktemp` que permitem personalizar ainda mais a criação de um arquivo temporário. Consulte a [documentação do Fish Shell](https://fishshell.com/docs/current/cmds/mktemp.html) para obter mais informações sobre essas opções.

## Veja também

- [Documentação do Fish Shell sobre o comando `mktemp`](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Exemplos de uso do comando `mktemp`](https://ostechnix.com/how-to-create-a-temporary-file-and-directory-in-linux/)