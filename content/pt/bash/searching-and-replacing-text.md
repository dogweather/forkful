---
title:                "Buscando e substituindo texto"
html_title:           "Bash: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por quê?

"Buscar e substituir" é uma função comum entre os programadores que permite alterar texto em um arquivo ou documento. Isso pode ser útil para fazer correções rápidas em um código ou para substituir uma palavra ou trecho em várias linhas de uma só vez. É uma maneira eficiente de fazer alterações em grande escala em um arquivo.

## Como fazer:

Para buscar e substituir texto em Bash, você pode usar o comando "sed". Aqui está um exemplo simples:

```Bash
sed 's/velha/nova/g' arquivo.txt
```

Este comando irá substituir a palavra "velha" por "nova" em todo o arquivo "arquivo.txt". O "g" no final indica que a substituição deve ser feita em todas as ocorrências da palavra no arquivo.

Você também pode usar expressões regulares para fazer substituições mais complexas. Por exemplo, para substituir todas as letras maiúsculas por minúsculas, você pode usar o seguinte comando:

```Bash
sed 's/[A-Z]/[a-z]/g' arquivo.txt
```

Este comando usaria uma expressão regular para identificar todas as letras maiúsculas e substituí-las por letras minúsculas. Isso pode ser útil para padronizar o texto em um arquivo.

## Profundidade:

[Buscar e substituir] tem sido uma função essencial para programadores por décadas. Existe desde a criação do editor de texto "ed" em 1971. Além do comando "sed", existem outras ferramentas que podem ser usadas para buscar e substituir texto em Bash, como "awk" e "grep". Cada uma dessas ferramentas tem suas próprias funcionalidades e é importante entender suas diferenças para escolher a ferramenta certa para cada situação.

## Veja também:

- [Artigo sobre "sed" no Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_02.html)
- [Documentação sobre expressões regulares em Bash](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html#Pattern-Matching)