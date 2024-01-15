---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

A criação de arquivos temporários é uma técnica comumente utilizada na programação Bash para armazenar informações temporárias que podem ser facilmente descartadas após o seu uso. Isso ajuda a manter o ambiente limpo e organizado, além de evitar conflitos com arquivos permanentes.

## Como Fazer

Para criar um arquivo temporário em Bash, podemos usar o comando `mktemp` seguido pelo prefixo "tmp" que será adicionado ao nome do arquivo. Por exemplo:

```Bash
tmp_file=$(mktemp tmp.XXXXXX)
```

Este comando criará um arquivo com um nome aleatório, que começa com "tmp" e é seguido por seis caracteres aleatórios. Podemos então usar essa variável `tmp_file` em nosso script para armazenar dados temporários.

Podemos também especificar o diretório onde queremos criar o arquivo temporário, usando a opção `-p`. Por exemplo:

```Bash
tmp_file=$(mktemp -p /home/usuario/tmp/ tmp.XXXXXX)
```

Isso criará o arquivo temporário no diretório `/home/usuario/tmp/` com o mesmo nome aleatório.

Podemos também usar o comando `touch` para criar um arquivo vazio e, em seguida, usá-lo como arquivo temporário. Por exemplo:

```Bash
touch tmp_file
```

Também podemos adicionar informações ao arquivo temporário usando redirecionamento de saída, como por exemplo:

```Bash
echo "Informações temporárias" > tmp_file
```

Depois de utilizarmos o arquivo temporário, é recomendado excluir ele usando o comando `rm`, para evitar a sobrecarga de arquivos desnecessários no nosso sistema. Por exemplo:

```Bash
rm tmp_file
```

## Mergulho Profundo

Ao criarmos um arquivo temporário usando `mktemp`, ele automaticamente será atribuído com permissão de leitura e escrita para o usuário atual e permissão de leitura para o grupo e outros usuários. Isso ajuda a garantir a segurança e privacidade dos dados armazenados no arquivo.

Podemos usar a opção `-d` com o comando `mktemp` para criar um diretório temporário em vez de um arquivo. Isso pode ser útil para armazenar vários arquivos temporários relacionados em um mesmo diretório.

Além disso, podemos usar a opção `-t` para especificar um prefixo personalizado para o nome do arquivo ou diretório temporário, ao invés do padrão "tmp".

## Veja Também

- [Documentação Bash - mktemp](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html)
- [Como usar o redirecionamento de saída em Bash](https://www.digitalocean.com/community/tutorials/como-usar-o-redirecionamento-de-saida-em-bash)