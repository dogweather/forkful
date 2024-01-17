---
title:                "Removendo caracteres que correspondem a um padrão"
html_title:           "Bash: Removendo caracteres que correspondem a um padrão"
simple_title:         "Removendo caracteres que correspondem a um padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

O que e por que?

Deletar caracteres que correspondem a um padrão é uma tarefa comum na programação Bash, que permite que você remova um ou mais caracteres específicos de uma string. Os programadores geralmente fazem isso para limpar dados ou formatar entradas de usuário corretamente.

Como fazer:

Você pode usar o comando "sed" para deletar caracteres que correspondem a um padrão em Bash. Por exemplo, se você quiser remover todos os números de uma string, você pode usar o seguinte comando:

```
$ echo "123abc456" | sed 's/[0-9]*//g'
abc
```

Neste comando, usamos a opção "s" para substituir qualquer caractere que corresponda ao padrão [0-9] pelo caractere vazio. A opção "g" é usada para garantir que todos os caracteres correspondentes sejam removidos, não apenas o primeiro.

Para tornar o comando mais flexível, você pode substituir o padrão [0-9] por qualquer outro padrão que desejar, como [A-Z] para remover todas as letras maiúsculas ou [^a-z] para remover tudo, exceto letras minúsculas.

Profundando:

O comando "sed" (abreviação para "editor de fluxo") é uma ferramenta de linha de comando que pode ser usada para manipular textos em UNIX e outras plataformas. Foi criado em 1974 e ainda é amplamente utilizado em sistemas operacionais UNIX e em linguagens de programação como Bash.

Uma alternativa comum ao comando "sed" é o "awk", que também permite a manipulação de texto. No entanto, o "awk" é mais adequado para o processamento de grandes quantidades de dados, enquanto o "sed" é mais rápido e eficiente para a manipulação de cadeias de caracteres menores.

Você também pode usar expressões regulares (regex) para corresponder a padrões e deletar caracteres em Bash. Isso pode ser útil quando você precisa de mais flexibilidade e precisão na manipulação do texto.

Veja também:

Para mais informações sobre o comando "sed", você pode consultar a documentação oficial em: https://www.gnu.org/software/sed/ ou verificar o manual do usuário digitando "man sed" no terminal Bash.

Para aprender mais sobre expressões regulares em Bash, você pode conferir este guia do The Linux Documentation Project: http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_01.html.