---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Extração de substrings é o processo de abstração de uma parte específica de uma string completa. Programadores utilizam isso para limpar, segmentar e manipular dados para atingir um objetivo específico.

##Como Fazer:

Extrair substrings em Bash é notavelmente simples. Tudo que você precisa conhecer é a posição inicial e o comprimento da subcadeia que você quer extrair. Confira uma implementação básica abaixo:

```Bash
meu_texto="O amor é um pássaro rebelde"
echo ${meu_texto:5:10}
```

A operação acima irá imprimir "é um pássa", pois é a substring começando na quinta posição e com comprimento de 10 caracteres.

## Mergulho Profundo:

O uso dessas substrings não é algo novo, elas são usadas desde os primeiros dias da programação para manipulação de dados. Alternativas para a extração de substrings em Bash incluem o uso de `cut`, `awk`, ou `sed`, cada um tendo suas próprias peculiaridades na sintaxe e implementação. O método que utilizamos aqui (`${string:position:length}`) é aceito nativamente pelo Bash sem a necessidade de invocar qualquer outra ferramenta externa.

## Ver Também:

Para mais profundidade no assunto, dê uma olha nestas fontes:

1. [Guia Avançado de Scripting Bash](https://tldp.org/LDP/abs/html/string-manipulation.html): inclui todos os conceitos avançados sobre manipulação de strings em Bash.
2. [Manual do Bash](https://www.gnu.org/software/bash/manual/bash.html): a documentação oficial do Bash é sempre o recurso mais confiável.
3. [StackOverflow](https://stackoverflow.com/): Uma rica comunidade de programadores, onde você pode encontrar muitas discussões em torno da manipulação de cadeias de caracteres em Bash.