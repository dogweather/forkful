---
title:                "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair subcadeias de caracteres é útil

Extrair subcadeias de caracteres é uma técnica útil em programação Bash para obter partes específicas de uma string maior. Isso pode ser útil em diversas situações, como manipulação de dados, formatação de strings e filtragem de informações.

## Como fazer a extração de substrings

Para extrair substrings em Bash, utilizamos o comando `cut` seguido de opções que indicam o início e fim da substring desejada. Por exemplo, se quisermos extrair os três primeiros caracteres de uma string, podemos usar o comando `cut -c 1-3`. O `1-3` indica que queremos os caracteres que vão do primeiro ao terceiro.

Veja um exemplo de código abaixo:

```Bash
# definindo a string
string="Programação Bash é divertida"

# extraindo as três primeiras letras
substring=$(echo $string | cut -c 1-3)
echo $substring
# saída: Pro
```

Também é possível utilizar o `cut` para extrair a substring a partir de uma posição específica. Por exemplo, se quisermos extrair os caracteres a partir do sétimo, podemos usar `cut -c 7-`. O `-` sozinho indica que queremos todos os caracteres a partir da sétima posição.

Além disso, o `cut` também permite extrair substrings baseadas em delimitadores, utilizando a opção `-d`. Um exemplo disso seria extrair apenas o domínio de um endereço de e-mail, utilizando o `@` como delimitador.

## Aprofundando na extração de substrings

O comando `cut` também possui outras opções que permitem uma maior flexibilidade na extração de substrings. Por exemplo, podemos utilizar a opção `-f` para extrair campos específicos de uma string, delimitados por espaços ou tabs.

Outra opção útil é o `-s`, que permite ignorar linhas que não possuem o delimitador especificado. Isso pode ser útil quando estamos trabalhando com arquivos de dados com linhas inconsistentes.

Com um pouco de prática e compreensão dessas opções, podemos extrair substrings de maneira eficiente e flexível em nossos scripts Bash.

## Veja também

[A documentation on the `cut` command](https://www.computerhope.com/unix/ucut.htm)

[A Bash tutorial on string manipulation](https://linuxconfig.org/bash-scripting-tutorial-for-beginners#h1--working-with-strings)