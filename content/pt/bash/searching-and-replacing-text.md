---
title:                "Bash: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A busca e substituição de texto é uma tarefa fundamental na programação. Ela nos permite automatizar a alteração de pedaços de texto em nosso código e nos ajuda a economizar tempo e esforço manual. Além disso, é uma habilidade essencial para qualquer programador que deseja trabalhar de forma eficiente e produtiva.

## Como fazer

Para realizar uma busca e substituição de texto no Bash, você pode usar o comando `sed`. Este comando é usado para substituir padrões em linhas de texto em arquivos ou mesmo em strings. Aqui está um exemplo básico de como usá-lo:

```Bash
sed 's/padrões/nova_substituição/' arquivo.txt
```

Neste exemplo, "arquivo.txt" é o nome do arquivo que você deseja alterar e "padrões" é o texto que você deseja substituir, enquanto "nova_substituição" é o novo texto que você deseja inserir no lugar. Você pode executar este comando em um terminal para ver o resultado e confirmar se a substituição foi feita corretamente. 

Se você deseja substituir o texto em vários arquivos, pode usar o comando `find` para especificar quais arquivos deseja alterar. Aqui está um exemplo:

```Bash
find . -type f -exec sed -i 's/padrões/nova_substituição/' {} ;
```

Este comando encontra todos os arquivos no diretório atual e seus subdiretórios e substitui o texto "padrões" por "nova_substituição" em todos os arquivos.

## Mergulhe mais fundo

O comando `sed` é poderoso e oferece muitas opções para personalizar sua busca e substituição de texto. Por exemplo, você pode usar expressões regulares para especificar padrões de texto mais complexos. Também é possível usar a opção `-i` para alterar os arquivos diretamente, sem a necessidade de criar um arquivo de saída separado. Isso ajuda a automatizar ainda mais o processo e economizar tempo.

Existem também outras ferramentas de busca e substituição de texto no Bash, como o comando `awk`, que também é capaz de encontrar e substituir texto em arquivos ou strings. Pesquise mais sobre essas ferramentas para descobrir mais opções e recursos.

## Ver também

- [Documentação oficial do comando `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial sobre busca e substituição de texto no Bash](https://www.tecmint.com/find-replace-string-in-files-linux/)
- [Diferença entre expressões regulares básicas e estendidas](https://www.rexegg.com/regex-quickstart.html)