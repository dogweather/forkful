---
title:                "Extraindo Substrings"
html_title:           "Bash: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Extrair substrings é um processo em que um programador seleciona um pedaço específico de uma cadeia de caracteres (string). Isso pode ser útil para manipular dados de maneira mais precisa e eficiente em um programa.

## Como fazer:

Você pode extrair substrings em Bash usando a estrutura de comando "cut". Aqui está um exemplo simples:

```
frutas="maça laranja banana uva"
echo ${frutas:6:6}
```

Isso retornará "laranja", que é uma substring de 6 caracteres começando no sexto índice da variável de string "frutas". Você também pode usar o comando "grep" para extrair substrings com base em um padrão específico.

```
texto="Olá, meu nome é João"
echo $texto | grep -o '[a-z]\+$'
```

Isso retornará "João", a última palavra da variável "texto". Experimente diferentes opções de comandos para extrair diferentes substrings.

## Mergulho profundo:

Extrair substrings em Bash tem sido uma técnica comum desde os primeiros dias do shell Unix. Em linguagens de programação mais novas, como Python, existem funções específicas para extrair substrings, mas alguns programadores ainda preferem a simplicidade e familiaridade do Bash.

Além do "cut" e "grep", você também pode usar o comando "awk" para extrair substrings de maneira mais complexa. Ele permite que você selecione campos específicos de uma cadeia de caracteres com base em um delimitador.

## Veja também:

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/)
- [Tutorial de substring em Bash](https://www.linux.com/training-tutorials/how-use-awk-beginners-guide/)
- [Outras maneiras de manipular strings em Bash](https://www.bash.academy/tema-strings/)